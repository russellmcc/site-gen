{-# OPTIONS_GHC -F -pgmF she #-}
{-# language UnicodeSyntax, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module CssProcess (
                   process
                  ) where
import Prelude hiding (takeWhile, take, all, foldr)
import Data.Text hiding (takeWhile, foldr, take, count, reverse, head, empty, all)
import Util
import Data.Attoparsec.Text hiding (skip)
import Data.Attoparsec.Combinator
import Control.Applicative
import Data.Foldable
import Data.Maybe
import qualified Data.Map as M
import Data.List hiding (insert, foldr)
import Data.Monoid
import Control.Monad
import CssTokenizer
import Text.Parsec.Pos
import Data.Functor.Identity
import Data.Tuple
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as E

newtype TryParse s u m a = TryParse (P.ParsecT s u m a) deriving (Functor, Monad, Applicative, MonadPlus)
getTryParse (TryParse a) = a
tryTryParse (TryParse a) = P.try a

instance Alternative (TryParse s u m) where
    empty = TryParse P.parserZero
    a <|> b = TryParse (P.try (getTryParse a) P.<|> P.try (getTryParse b))

runTP p u = P.runPT (getTryParse p) u ""

getState' ∷ (P.Stream a b c) ⇒ Processor a ProcessState b
getState' = TryParse $ P.getState
putState' a = TryParse $ (P.putState a)

buildExpressionParser' table term = TryParse $ E.buildExpressionParser table $ getTryParse term

many' a = TryParse $ (P.many . getTryParse) a
sepByT a b = TryParse $ (P.sepBy (getTryParse a) (getTryParse b))
many1' a = TryParse $ (P.many1 . getTryParse) a

eof' = TryParse P.eof

data ProcessState = ProcessState {
      dollarDecls :: M.Map String ([CssToken]), 
      mixinDecls :: M.Map String ([String], [AtRuleElem])
}
pDollarInsert ident v (ProcessState d m) =  ProcessState (M.insert ident v d) m
pMixinInsert ident v (ProcessState d m) =  ProcessState d  $ M.insert ident v m
pDollarLookup ident p = M.lookup ident $ dollarDecls p
pDollarDelete ident (ProcessState d m) = ProcessState (M.delete ident d) m
pMixinDelete ident (ProcessState d m) = ProcessState d $ M.delete ident m
pMixinLookup ident p = M.lookup ident $ mixinDecls p
pEmpty = ProcessState M.empty M.empty

type Processor s t b = TryParse s ProcessState b t

-- CSS specific parser
cssTok f = TryParse $ P.tokenPrim show (\x _ _ → x) f
cssTokOp f = P.tokenPrim show (\x _ _ → x) f

skip ∷ (Show a, Applicative f, P.Stream (f a) b a) ⇒ Processor (f a) (f a) b
skip = cssTok (pure . pure)

dollarDecl ∷ (P.Stream a b c) ⇒ (String, [CssToken]) → Processor a [d] b
dollarDecl (ident, values) = getState' >>= 
                          (putState' . (pDollarInsert ident values)) >>
                          (|[]|)

mixinDecl ∷ (P.Stream a b c) ⇒ ([CssToken], Maybe [AtRuleElem]) → Processor a [d] b
mixinDecl (preface, block) = doMixin *> (|[]|)
    where
      doMixin ∷ (P.Stream a b c) ⇒ (Processor a () b)
      doMixin = fromMaybe (|()|) $ addMixin <$> checkMixin

      addMixin (s, as, rs) = getState' >>= (putState' . (pMixinInsert s (as, rs)))
      checkMixin ∷ Maybe (String, [String], [AtRuleElem])
      checkMixin = (|(,,) (checkName preface) (checkArgs preface) checkRules|)
      checkName ((CssIdent i): _) = (|i|)
      checkName ((CssFunction i _): _) = (|i|)
      checkName _ = (|)
      checkArgs ((CssFunction _ args):_) = (|(asum $ (|dollarFilt args|))|)
      checkArgs _ = (|[]|)
      dollarFilt (CssDollarKeyword d) = (|d|)
      dollarFilt _ = (|)
      checkRules = (|(asum $ toList $ block)|)

processRule ∷ ProcessState → Rule → Rule
processRule s (Rule (sel, elems)) = Rule (sel, snd $ procRuleElems s elems)

checkRight ∷ a → Either b a → a
checkRight t (Left _) = t
checkRight _ (Right t) = t

runSubParser p s d vs = checkRight d $ runIdentity $ runTP p s vs

mathOp ∷ CssToken → (Double → Double → Double) → E.Operator [Maybe CssToken] ProcessState Identity (Maybe CssToken)
mathOp t m = E.Infix (|(%cssTokOp $ only (|t|) %) (math' m)|) E.AssocLeft
    where only a b = case (a == b) of
                       True -> (|a|)
                       False -> (|)
          math ∷ (Double → Double → Double) → CssToken → CssToken → Maybe CssToken
          math f (CssPercentage t) b = math f (CssNumber $ t / 100) b
          math f b (CssPercentage t)  = math f b (CssNumber $ t / 100)
          math f (CssNumber a) (CssNumber b) = (|(CssNumber $ f a b)|)
          math f (CssDimension (a, ad)) (CssDimension (b, bd)) = case (ad == bd) of
                                                                   True → (|(CssDimension ((f a b), ad))|)
                                                                   False → (|)
          math f (CssNumber a) (CssDimension (b, d)) = (|(CssDimension (f a b, d))|)
          math f (CssDimension (a, d)) (CssNumber b) = (|(CssDimension (f a b, d))|)
          math _ _ _ = (|)
          math' ∷ (Double → Double → Double) → Maybe CssToken → Maybe CssToken → Maybe CssToken
          math' f a b = join $ (|(math f) a b|)

opTable = (|pure [ (mathOp (CssDelim "*") (*))
                 , (mathOp (CssDelim "/") (/))
                 , (mathOp (CssDelim "%") (modulo))                
                 , (mathOp (CssDelim "+") (+))
                 , (mathOp (CssDelim "-") (-))
                 ]|)
    where modulo a b = b * (snd $ properFraction (a / b))

evalValues ∷ ProcessState → [CssToken] → [CssToken]
evalValues s vs = eval (expand s vs)
    where 
      -- actually evaluates an expression.
      eval vs = runSubParser evalP pEmpty vs (|pure vs|)
      evalP ∷ TryParse [Maybe CssToken] ProcessState Identity [CssToken]
      evalP = (|pure (failOnNothing (buildExpressionParser' opTable (cssTok $ cssTerm))) (%eof'%)|)
      failOnNothing p = p >>= \x → case (x) of
                                     Just a → (|a|)
                                     Nothing → (|)

      -- expands all the variables in the expression
      expand s vs = runSubParser expandP s vs vs
      expandP = (|asum (many' procVal)|)
      procVal = replaceDollar <|> skip
      replaceDollar = getState' >>= \s → (cssTok $ checkDollar s)
      lookupDVal d s = (pDollarLookup d s) >>= pure
      evalValues' ∷ String → ProcessState → [CssToken] → [CssToken]
      evalValues' d s vs = evalValues (pDollarDelete d s) vs
      checkDollar s (CssDollarKeyword d) = (|(asum $ toList $ (|(evalValues' d s) (lookupDVal d s)|))|)
      checkDollar _ _ = (|)

      -- cssTerm needs to deal with special cases like parens
      cssTerm ∷ Maybe CssToken → Maybe (Maybe CssToken)
      cssTerm = ((|cssTerm'|) <*>)
      cssTerm' (CssParen vs) = failIfLen $ evalValues s vs             
      cssTerm' a = (|a|)

      failIfLen ∷ [CssToken] → Maybe CssToken
      failIfLen (a:[]) = (|a|)
      failIfLen _ = (|)

processDecl s (prop, vs) = check prop $ evalValues s vs
    where check _ [] = []
          check p vs = (|(p, vs)|)

expandIncludeForRule ∷ ProcessState → [CssToken] → (ProcessState, [RuleElem])
expandIncludeForRule state ts = (expandInclude isRuleElem procRuleElems state ts)
    where
      isRuleElem (AtRuleRuleElem r) = (|r|)
      isRuleElem _ = (|)

expandIncludeForAtRule ∷ ProcessState → [CssToken] → (ProcessState, [AtRuleElem])
expandIncludeForAtRule state ts = (expandInclude pure procAtRuleElems state ts)

expandInclude ∷ (AtRuleElem → [a]) → (ProcessState → [a] → (ProcessState, [a])) → ProcessState → [CssToken] → (ProcessState, [a])
expandInclude f proc state ts = package (|doExpand (getNameAndArgs ts)|)
    where
      doExpand (ident, args) = package' (ident, pMixinLookup ident state) (|proc newState (|ruleFilt (|snd (pMixinLookup ident state)|)|)|)
          where newState = (|pMixinDelete ~ident bindDollars|)
                -- Some complexity here because we're computing under two contexts
                dollarsToBind ∷ Maybe (ZipList String)
                dollarsToBind = (|ZipList (|fst (pMixinLookup ident state)|)|)
                valuesToBindTo ∷ Maybe (ZipList [CssToken])
                valuesToBindTo = (|ZipList ~(splitList args)|)
                bindDollarList ∷ Maybe [(ProcessState → ProcessState)]
                bindDollarList = (|getZipList (|(|(|pDollarInsert|) <$> dollarsToBind|) <*> valuesToBindTo|)|)
                bindDollars ∷ Maybe ProcessState
                bindDollars = (|appEndo (|fold (|(|Endo|) <$> bindDollarList|)|) ~state|)
                splitList ∷ [CssToken] → [[CssToken]]
                splitList args = runSubParser sepParse pEmpty (|args|) args
                sepParse = sepByT (many (cssTok notComma)) (cssTok isComma)
                isComma (CssDelim ",") = (|()|)
                isComma _ = (|)
                notComma c = case (isComma c) of
                               Just _ → (|)
                               Nothing → (|c|)

      getNameAndArgs ((CssIdent ident):_) = (|(ident, [])|)
      getNameAndArgs ((CssFunction ident args):_) = (|(ident, args)|)
      getNameAndArgs _ = (|)
      ruleFilt as = asum $ (|f as|)
      package (Just (s, rs)) = (s, rs)
      package  _ = (state, [])
      package' (i, Just t) (Just (s, rs)) = (pMixinInsert i t s, rs)
      package' _  _ = (state, [])

stateUpdate t = do
  (n, rs) ← (getState' >>= t) 
  putState' n
  return rs

procRuleElems ∷ ProcessState → [RuleElem] → (ProcessState, [RuleElem])
procRuleElems p rs = runSubParser parser p (p, rs) rs
    where 
      parser = (|swap (| (,) (|asum (many' ruleStep)|) getState'|)|)
      ruleStep = doInclude <|> doDecl <|> eatDollarDecl <|> skip
      doDecl = getState' >>= (cssTok . checkDecl)
      checkDecl s (RuleDecl (Declaration d)) = (|(|(RuleDecl . Declaration) (processDecl s d)|)|)
      checkDecl _ _ = (|)
      eatDollarDecl ∷ Processor [RuleElem] [RuleElem] Identity
      eatDollarDecl = (cssTok $ checkDollarDecl) >>= dollarDecl
      checkDollarDecl (RuleDollar (DollarDeclaration d)) = (|d|)
      checkDollarDecl _ = (|)
      doInclude = stateUpdate (cssTok . checkInclude)
      checkInclude s (RuleAtRule (AtRule ("include", args, (|)))) = (|(expandIncludeForRule s args)|)
      checkInclude _ _ = (|)

procAtRuleElems ∷ ProcessState → [AtRuleElem] → (ProcessState, [AtRuleElem])
procAtRuleElems p rs = runSubParser parser p (p, rs) rs
    where
      parser = (|swap (| (,) (|asum (many' ruleStep)|) getState'|)|)
      ruleStep = doInclude <|> doRule <|> doRuleElem <|> skip
      doRuleElem = (|(<$>) ~AtRuleRuleElem (stateUpdate (cssTok . checkRuleElem))|)
      checkRuleElem s (AtRuleRuleElem r) = (|(procRuleElems s [r])|)
      checkRuleElem _ _ = (|)
      doRule = getState' >>= (cssTok . checkRule)
      checkRule s (AtRuleRule r) = (|(|(AtRuleRule $ processRule s r)|)|)
      checkRule _ _ = (|)
      doInclude = stateUpdate (cssTok . checkInclude)
      checkInclude s (AtRuleRuleElem (RuleAtRule (AtRule ("include", args, (|))))) = (|(expandIncludeForAtRule s args)|)
      checkInclude _ _ = (|)

processAtRule ∷ ProcessState → AtRule → AtRule
processAtRule s (AtRule (typ, sel, (Just elems))) = AtRule (typ, sel, (|(snd $ procAtRuleElems s elems)|))
processAtRule s r = r

-- top-level processor.
processP ∷ Processor [StyleSheetElem] [StyleSheetElem] IO
processP = (|asum (many' stepP)|)
    where 
      stepP = eatDollarDecl <|> eatMixin <|> doRule <|> doAtRule <|> skip
      doRule = getState' >>= (cssTok . checkRule)
      doAtRule = getState' >>= (cssTok . checkAtRule)
      checkRule s (StyleRule r) = (|(|(StyleRule $ processRule s r)|)|)
      checkRule _ _ = (|)
      checkAtRule s (StyleAtRule r) = (|(|(StyleAtRule $ processAtRule s r)|)|)
      checkAtRule _ _ = (|)
      eatDollarDecl = (cssTok $ checkDollarDecl) >>= dollarDecl
      checkDollarDecl (StyleDollarDecl (DollarDeclaration d)) = (|d|)
      checkDollarDecl _ = (|)
      eatMixin = (cssTok $ checkMixin) >>= mixinDecl
      checkMixin (StyleAtRule (AtRule ("mixin", s, b))) = (|(s,b)|)
      checkMixin _ = (|)

process ∷ Either String [StyleSheetElem] → IO (Either String [StyleSheetElem])
process = either (return . Left) (\x → showErr <$> (runTP processP pEmpty x))
    where showErr ∷ Show a ⇒ Either a b → Either String b
          showErr (Left e) = Left $ show e
          showErr (Right b) = Right b

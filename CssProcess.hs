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

many'' a = TryParse $ (P.many . getTryParse) a
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
                          (pure [])
mixinDecl ∷ (P.Stream a b c) ⇒ ([CssToken], Maybe [AtRuleElem]) → Processor a [d] b
mixinDecl (preface, block) = doMixin *> (pure [])
    where
      doMixin ∷ (P.Stream a b c) ⇒ (Processor a () b)
      doMixin = fromMaybe (pure ()) $ addMixin <$> checkMixin
      addMixin (s, as, rs) = getState' >>= (putState' . (pMixinInsert s (as, rs)))
      checkMixin ∷ Maybe (String, [String], [AtRuleElem])
      checkMixin = ((((pure (,,)) <*> (checkName preface)) <*> (checkArgs preface)) <*> checkRules)
      checkName ((CssIdent i): _) = (pure i)
      checkName ((CssFunction i _): _) = (pure i)
      checkName _ = empty
      checkArgs ((CssFunction _ args):_) = (pure (asum $ ((pure dollarFilt) <*> args)))
      checkArgs _ = (pure [])
      dollarFilt (CssDollarKeyword d) = (pure d)
      dollarFilt _ = empty
      checkRules = (pure (asum $ toList $ block))
processRule ∷ ProcessState → Rule → Rule
processRule s (Rule (sel, elems)) = Rule (sel, snd $ procRuleElems s elems)
checkRight ∷ a → Either b a → a
checkRight t (Left _) = t
checkRight _ (Right t) = t
runSubParser p s d vs = checkRight d $ runIdentity $ runTP p s vs
mathOp ∷ CssToken → (Double → Double → Double) → E.Operator [Maybe CssToken] ProcessState Identity (Maybe CssToken)
mathOp t m = E.Infix ((cssTokOp $ only (pure t) ) *> (pure (math' m))) E.AssocLeft
    where only a b = case (a == b) of
                       True -> (pure a)
                       False -> empty
          math ∷ (Double → Double → Double) → CssToken → CssToken → Maybe CssToken
          math f (CssPercentage t) b = math f (CssNumber $ t / 100) b
          math f b (CssPercentage t)  = math f b (CssNumber $ t / 100)
          math f (CssNumber a) (CssNumber b) = (pure (CssNumber $ f a b))
          math f (CssDimension (a, ad)) (CssDimension (b, bd)) = case (ad == bd) of
                                                                   True → (pure (CssDimension ((f a b), ad)))
                                                                   False → empty
          math f (CssNumber a) (CssDimension (b, d)) = (pure (CssDimension (f a b, d)))
          math f (CssDimension (a, d)) (CssNumber b) = (pure (CssDimension (f a b, d)))
          math _ _ _ = empty
          math' ∷ (Double → Double → Double) → Maybe CssToken → Maybe CssToken → Maybe CssToken
          math' f a b = join $ (((pure (math f)) <*> a) <*> b)
opTable = ((pure pure) <*> [ (mathOp (CssDelim "*") (*))
                 , (mathOp (CssDelim "/") (/))
                 , (mathOp (CssDelim "%") (modulo))                
                 , (mathOp (CssDelim "+") (+))
                 , (mathOp (CssDelim "-") (-))
                 ])
    where modulo a b = b * (snd $ properFraction (a / b))
evalValues ∷ ProcessState → [CssToken] → [CssToken]
evalValues s vs = eval (expand s vs)
    where 
      -- actually evaluates an expression.
      eval vs = runSubParser evalP pEmpty vs ((pure pure) <*> vs)
      evalP ∷ TryParse [Maybe CssToken] ProcessState Identity [CssToken]
      evalP = (((pure pure) <*> (failOnNothing (buildExpressionParser' opTable (cssTok $ cssTerm)))) <* (eof'))
      failOnNothing p = p >>= \x → case (x) of
                                     Just a → (pure a)
                                     Nothing → empty
      -- expands all the variables in the expression
      expand s vs = runSubParser expandP s vs vs
      expandP = ((pure asum) <*> (many'' procVal))
      procVal = replaceDollar <|> skip
      replaceDollar = getState' >>= \s → (cssTok $ checkDollar s)
      lookupDVal d s = (pDollarLookup d s) >>= pure
      evalValues' ∷ String → ProcessState → [CssToken] → [CssToken]
      evalValues' d s vs = evalValues (pDollarDelete d s) vs
      checkDollar s (CssDollarKeyword d) = (pure (asum $ toList $ ((pure (evalValues' d s)) <*> (lookupDVal d s))))
      checkDollar _ _ = empty
      -- cssTerm needs to deal with special cases like parens
      cssTerm ∷ Maybe CssToken → Maybe (Maybe CssToken)
      cssTerm = ((pure cssTerm') <*>)
      cssTerm' (CssParen vs) = failIfLen $ evalValues s vs             
      cssTerm' a = (pure a)
      failIfLen ∷ [CssToken] → Maybe CssToken
      failIfLen (a:[]) = (pure a)
      failIfLen _ = empty
processDecl s (prop, vs) = check prop $ evalValues s vs
    where check _ [] = []
          check p vs = (pure (p, vs))
expandIncludeForRule ∷ ProcessState → [CssToken] → (ProcessState, [RuleElem])
expandIncludeForRule state ts = (expandInclude isRuleElem procRuleElems state ts)
    where
      isRuleElem (AtRuleRuleElem r) = (pure r)
      isRuleElem _ = empty
expandIncludeForAtRule ∷ ProcessState → [CssToken] → (ProcessState, [AtRuleElem])
expandIncludeForAtRule state ts = (expandInclude pure procAtRuleElems state ts)
expandInclude ∷ (AtRuleElem → [a]) → (ProcessState → [a] → (ProcessState, [a])) → ProcessState → [CssToken] → (ProcessState, [a])
expandInclude f proc state ts = package ((pure doExpand) <*> (getNameAndArgs ts))
    where
      doExpand (ident, args) = package' (ident, pMixinLookup ident state) (((pure proc) <*> newState) <*> ((pure ruleFilt) <*> ((pure snd) <*> (pMixinLookup ident state))))
          where newState = (((pure pMixinDelete) <*> (pure ident)) <*> bindDollars)
                -- Some complexity here because we're computing under two contexts
                dollarsToBind ∷ Maybe (ZipList String)
                dollarsToBind = ((pure ZipList) <*> ((pure fst) <*> (pMixinLookup ident state)))
                valuesToBindTo ∷ Maybe (ZipList [CssToken])
                valuesToBindTo = ((pure ZipList) <*> (pure (splitList args)))
                bindDollarList ∷ Maybe [(ProcessState → ProcessState)]
                bindDollarList = ((pure getZipList) <*> (((pure (<*>)) <*> ((((pure (<$>)) <*> ((pure pDollarInsert) )) <*> (dollarsToBind)) )) <*> (valuesToBindTo)))
                bindDollars ∷ Maybe ProcessState
                bindDollars = (((pure appEndo) <*> ((pure fold) <*> (((pure (<$>)) <*> ((pure Endo) )) <*> (bindDollarList)))) <*> (pure state))
                splitList ∷ [CssToken] → [[CssToken]]
                splitList args = runSubParser sepParse pEmpty (pure args) args
                sepParse = sepByT (many (cssTok notComma)) (cssTok isComma)
                isComma (CssDelim ",") = (pure ())
                isComma _ = empty
                notComma c = case (isComma c) of
                               Just _ → empty
                               Nothing → (pure c)
      getNameAndArgs ((CssIdent ident):_) = (pure (ident, []))
      getNameAndArgs ((CssFunction ident args):_) = (pure (ident, args))
      getNameAndArgs _ = empty
      ruleFilt as = asum $ ((pure f) <*> as)
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
      parser = ((pure swap) <*> (((pure (,)) <*> ((pure asum) <*> (many'' ruleStep))) <*> getState'))
      ruleStep = doInclude <|> doDecl <|> eatDollarDecl <|> skip
      doDecl = getState' >>= (cssTok . checkDecl)
      checkDecl s (RuleDecl (Declaration d)) = (pure ((pure (RuleDecl . Declaration)) <*> (processDecl s d)))
      checkDecl _ _ = empty
      eatDollarDecl ∷ Processor [RuleElem] [RuleElem] Identity
      eatDollarDecl = (cssTok $ checkDollarDecl) >>= dollarDecl
      checkDollarDecl (RuleDollar (DollarDeclaration d)) = (pure d)
      checkDollarDecl _ = empty
      doInclude = stateUpdate (cssTok . checkInclude)
      checkInclude s (RuleAtRule (AtRule ("include", args, empty))) = (pure (expandIncludeForRule s args))
      checkInclude _ _ = empty

procAtRuleElems ∷ ProcessState → [AtRuleElem] → (ProcessState, [AtRuleElem])
procAtRuleElems p rs = runSubParser parser p (p, rs) rs
    where
      parser = ((pure swap) <*> (((pure (,)) <*> ((pure asum) <*> (many'' ruleStep))) <*> getState'))
      ruleStep = doInclude <|> doRule <|> doRuleElem <|> skip
      doRuleElem = (((pure (<$>)) <*> (pure AtRuleRuleElem)) <*> (stateUpdate (cssTok . checkRuleElem)))
      checkRuleElem s (AtRuleRuleElem r) = (pure (procRuleElems s [r]))
      checkRuleElem _ _ = empty
      doRule = getState' >>= (cssTok . checkRule)
      checkRule s (AtRuleRule r) = (pure (pure (AtRuleRule $ processRule s r)))
      checkRule _ _ = empty
      doInclude = stateUpdate (cssTok . checkInclude)
      checkInclude s (AtRuleRuleElem (RuleAtRule (AtRule ("include", args, empty)))) = (pure (expandIncludeForAtRule s args))
      checkInclude _ _ = empty
processAtRule ∷ ProcessState → AtRule → AtRule
processAtRule s (AtRule (typ, sel, (Just elems))) = AtRule (typ, sel, (pure (snd $ procAtRuleElems s elems)))
processAtRule s r = r

-- top-level processor.
processP ∷ Processor [StyleSheetElem] [StyleSheetElem] IO
processP = ((pure asum) <*> (many'' stepP))
    where 
      stepP = eatDollarDecl <|> eatMixin <|> doRule <|> doAtRule <|> skip
      doRule = getState' >>= (cssTok . checkRule)
      doAtRule = getState' >>= (cssTok . checkAtRule)
      checkRule s (StyleRule r) = (pure (pure (StyleRule $ processRule s r)))
      checkRule _ _ = empty
      checkAtRule s (StyleAtRule r) = (pure (pure (StyleAtRule $ processAtRule s r)))
      checkAtRule _ _ = empty
      eatDollarDecl = (cssTok $ checkDollarDecl) >>= dollarDecl
      checkDollarDecl (StyleDollarDecl (DollarDeclaration d)) = (pure d)
      checkDollarDecl _ = empty
      eatMixin = (cssTok $ checkMixin) >>= mixinDecl
      checkMixin (StyleAtRule (AtRule ("mixin", s, b))) = (pure (s,b))
      checkMixin _ = empty

process ∷ Either String [StyleSheetElem] → IO (Either String [StyleSheetElem])
process = either (return . Left) (\x → showErr <$> (runTP processP pEmpty x))
    where showErr ∷ Show a ⇒ Either a b → Either String b
          showErr (Left e) = Left $ show e
          showErr (Right b) = Right b

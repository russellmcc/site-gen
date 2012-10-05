{-# language UnicodeSyntax #-}
module CssTokenizer (
                     CssToken (..),
                     Declaration (..),
                     Rule (..),
                     AtRuleElem (..),
                     AtRule (..),
                     StyleSheetElem (..),
                     DollarDeclaration (..),
                     RuleElem (..),
                     styleSheet
                    ) where
import Prelude hiding (takeWhile, take, all, elem)
import Data.Text hiding (tail, takeWhile, take, count, reverse, head, empty, all)
import Util
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Control.Applicative
import Data.Foldable
import Data.Maybe
import Data.Traversable
import Data.Monoid
import Control.Monad

-- some predicates
isNonAscii = notInClass "\0-\237"
isNmChar = inClass "_a-z0-9A-Z-"
isNmStart = inClass "_a-zA-Z"

-- escape characters
escapedChar = char '\\' <<:>> satisfy' (notInClass "\n\r\f0-9a-f")
escapedUnicode = char '\\' <<:>> counts (reverse [1..6]) unicodeChar
    where unicodeChar = satisfy $ inClass "0-9a-f"
escape = escapedChar <|> escapedUnicode

-- basic units
nmstart = satisfy' isNmStart <|> satisfy' isNonAscii <|> escape
nmchar = satisfy' isNmChar <|> satisfy' isNonAscii <|> escape
anyString s = unpack <$> asum (string' <$> s)
nl = anyString ["\n", "\r\n", "\r", "\f"] *> pure ()
wsClass = " \t\r\n\f"
ws = satisfy' (inClass wsClass) *> pure ()
skipSpace' = takeWhile (inClass wsClass) *> pure () -- css has different definition of whitespace than attoparsec.
endChar c = (char c *> pure ()) <|> (skipSpace' *> endOfInput)

-- single or double quoted string.

stringInside e = satisfy' (notInClass (e : "\n\r\f\\")) <|> (char '\\' <<:>> (nl *> pure "\n")) <|> escape

cssString = string1 <|> string2
    where string1 = asum <$> (char '"' *> many (stringInside '"') <* endChar '"')
          string2 = asum <$> (char '\'' *> many (stringInside '\'') <* endChar '\'')

badCssString = string1 <|> string2
    where string1 = char '"' *> many (stringInside '"') *> (nl <|> endOfInput)
          string2 = char '\'' *> many (stringInside '\'') *> (nl <|> endOfInput)

-- An "Identifier" is a nmstart wtih a collection of nmchars
ident = optSatisfy ('-' ==) <<|>> nmstart <<|>> (asum <$> many nmchar)

-- An "At-keyword" is an @ followed by an ident
atKeyword = char '@' *>  ident

-- A "dollar-keyword" is a $ followed by an ident
dollarKeyword = char '$' *> ident

-- A hash is a # followed by a name.
hash = char '#' *> (asum <$> many nmchar)

-- in the CSS, "num" is the same as attoparsec's "decimal.
num = double

-- a "percentage" is a decimal followed by the percent sign.
percentage = double <* char '%'

-- a "dimension" is a decimal followed by an identifier with no space
dimension = (,) <$> double <*> ident

-- a css url
url = string' "url(" *> skipSpace' *> (internalUrl1 <|> internalUrl2) <* skipSpace' <* endChar ')'
    where internalUrl1 = cssString
          internalUrl2 = asum <$> many (satisfy' (inClass "!#$%&*-[]-~") <|> satisfy' isNonAscii <|> escape)

-- we throw out the inside of the comment.
comment = comment1 <|> comment2
    where comment1 = string' "/*" *> manyTill anyChar (try $ string' "*/") *> pure ()
          comment2 = string' "//" *> manyTill anyChar (try nl) *> pure ()


unicodeRange = string' "u+" *> counts (reverse [1..6]) unicodeChar <<|>> option "" (char '-' <<:>> counts (reverse [1..6]) unicodeChar)
    where unicodeChar = satisfy $ inClass "0-9a-f"

-- okay, now we tokenize the whole file according to the grammar.  This is a superset of the css that we can understand.  We do this so we can fall back gracefully if it's a future version of CSS.

-- this is stuff that can live inside blocks.
data CssToken = CssIdent String | CssDollarKeyword String| CssNumber Double | CssPercentage Double | CssDimension (Double, String) | CssString String | CssURI String | CssHash String | CssUnicodeRange String | CssIncludes | CssDashMatch | CssColon | CssFunction String [CssToken] | CssParen [CssToken] | CssBracket [CssToken] |  CssDelim String | CssBlock [CssToken] | CssAtKeyword String | CssSemiColon deriving (Eq)

showNum = showPropFrac . properFraction
     where
       showPropFrac (a, b) = case (b == 0) of 
                               True -> show a
                               False -> case (a == 0) of
                                          True -> show b
                                          False -> show a ++ (tail $ show b)

noSpaces = [CssColon]
identSuffix x = case x `elem` noSpaces of
                   True → ""
                   False → " "

instance Show CssToken where
    show (CssIdent s) = s
    show (CssNumber d) = showNum d ++ " "
    show (CssPercentage d) = showNum d ++ "%"
    show (CssDimension (n, d)) = showNum n ++ d ++ " "
    show (CssString s) = "'" ++ s ++ "'"
    show (CssURI u) = "url('" ++ u ++ "') "
    show (CssHash h) = " " ++ ('#' : h)
    show (CssUnicodeRange s) = "u+" ++ s
    show CssIncludes = "~= "
    show CssDashMatch = "|= "
    show CssColon = ":"
    show (CssFunction f cs) = f ++ "(" ++ show cs ++ ")"
    show (CssParen cs) = "(" ++ show cs ++ ")"
    show (CssBracket cs) = "[" ++ show cs ++ "]"
    show (CssDelim s) = s
    show (CssBlock cs) = "{" ++ show cs ++ "}"
    show (CssAtKeyword s) = ('@' : s)
    show (CssDollarKeyword s) = ('$' : s )
    show CssSemiColon = ";\n"
    showList ((CssIdent a):b:as) = mappend (mappend $ a ++ (identSuffix b) ++ (show b)) (showList as)
    showList ((CssHash a):b:as) = mappend (mappend $ "#" ++ a ++ (identSuffix b) ++ (show b)) (showList as)
    showList (a:as) = mappend (shows a) (showList as)
    showList [] = id
-- sequence under an applicative
liftSeq p = sequenceA <$> p

(<<*>>) ∷ (Applicative f, Applicative g) ⇒ f (a → b) → f (g a) → f (g b) 
a <<*>> b = (fmap <$> a) <*> b


-- general element.  This doesn't actually parse *anything*, just the "any" from the CSS spec.
anyCss ∷ Parser (Maybe CssToken)
anyCss = many (comment <* skipSpace') *> maybe' <* skipSpace'
    where maybe' = (Just <$> (CssUnicodeRange <$> unicodeRange)) <|>
                   (Just <$> (CssURI <$> url)) <|>
                   funcParse <|>
                   (Just <$> (CssIdent <$> ident)) <|>
                   (Just <$> (CssPercentage <$> percentage)) <|>
                   (Just <$> (CssDimension <$> dimension)) <|>
                   (Just <$> (CssHash <$> hash)) <|>
                   (Just <$> (CssNumber <$> double)) <|>
                   (Just <$> (string' "~=" *> pure CssIncludes)) <|>
                   (Just <$> (string' "|=" *> pure CssDashMatch)) <|>
                   (Just <$> (string' ":" *> pure CssColon))  <|>
                   (CssParen <<$>> (char '(' *> recParse <* endChar ')')) <|>
                   (Just <$> (CssDollarKeyword <$> dollarKeyword)) <|>
                   (Just <$> (CssString <$> cssString)) <|>
                   (badCssString *> pure Nothing) <|>
                   (CssBracket <<$>> (char '[' *> recParse <* endChar ']')) <|>
                   (Just <$> (CssDelim <$> satisfy' (notInClass ":;{}()[] \t\r\n\f")))
          recParse = liftSeq $ many (skipSpace *> anyCss)
          funcParse = (CssFunction <$> ident) <<*>> (char '(' *> recParse <* endChar ')')
          insideFuncParse = (many (skipSpace *> anyCss))

parseJust ∷ Parser (Maybe a) → Parser a
parseJust p = p >>= \a → case a of
                     Just b → pure b
                     Nothing → empty

-- "CSS Block" from the spec
cssBlockElem = (Just <$> (CssAtKeyword <$> (atKeyword <* skipSpace'))) <|>
               anyCss <|>
               (CssBlock <<$>> cssBlock) <|>
               (char ';' *> skipSpace' *> pure (Just CssSemiColon))

cssBlock = char '{' *>
           skipSpace' *>
           liftSeq (many cssBlockElem) <*
           skipSpace' <*
           endChar '}' <* skipSpace'

-- "Value" from the spec
value = (Just <$> (CssAtKeyword <$> (atKeyword <* skipSpace'))) <|>
        (Just <$> (CssDollarKeyword <$> (dollarKeyword <* skipSpace'))) <|>
        anyCss <|>
        (CssBlock <<$>> cssBlock)

newtype Declaration = Declaration (String, [CssToken]) deriving (Eq)
declaration = Declaration <<$>> (((,) <$> (ident <* skipSpace')) <<*>> (char ':' *> skipSpace' *> liftSeq (many1 value)))

instance Show Declaration where
    show (Declaration (t, vs)) = t ++ ": " ++ show vs
    showList ds = mconcat $ ((++ ";\n") . ) <$> (shows <$> ds)

newtype DollarDeclaration = DollarDeclaration (String, [CssToken]) deriving (Eq)
dollarDeclaration = char '$' *> DollarDeclaration <<$>> (((,) <$> (ident <* skipSpace')) <<*>> (char ':' *> skipSpace' *> liftSeq (many1 value)))

instance Show DollarDeclaration where
    show (DollarDeclaration (t, vs)) = "$" ++ t ++ ": " ++ show vs
    showList ds = mconcat $ ((++ ";\n") . ) <$> (shows <$> ds)

-- to help with crazy error case rules.  errors must still respect nesting of (), [], {}, "", and ''
badness ∷ String → Parser ()
badness inedibleClass = many (asum (badMatch <$> delimPairs) <|> satisfy' (notInClass (inedibleClass <|> (head <$> delimPairs))) *> pure ()) *> pure ()
          where badMatch (a:b:_) = char a *> badness [b] *> char b *> pure ()
                delimPairs = ["()", "[]", "{}", "\"\"", "''"]

data RuleElem = RuleDecl Declaration | RuleAtRule AtRule | RuleDollar DollarDeclaration deriving Eq
instance Show RuleElem where
    show (RuleDecl d) = show d ++ ";\n"
    show (RuleAtRule r) = show r
    show (RuleDollar d) = show d ++ ";\n"
    showList ds = mconcat $ shows <$> ds

newtype Rule = Rule ([CssToken], [RuleElem]) deriving (Eq)

instance Show Rule where
    show (Rule (ts,ds)) = show ts ++ "{\n" ++ show ds ++ "}\n"
    showList rs = mconcat $ shows <$> rs

rule = Rule <$> ((,) <$> (skipSpace' *> many (parseJust anyCss)) <*> ruleBlock)
ruleSemiColonSep = (RuleDecl <<$>> declaration) <|> (RuleDollar <<$>> dollarDeclaration)
ruleNoSep = (Just . RuleAtRule) <$> atRule
ruleBlock = char '{' *> skipSpace' *> declList <* skipSpace' <* endChar '}' <* skipSpace'
    where declList = join <$> (maybeToList <<$>> sepBy'' (ruleSemiColonSep) (char ';' *> skipSpace') (ruleNoSep <* skipSpace'))


data AtRuleElem = AtRuleRule Rule | AtRuleRuleElem RuleElem | AtRuleToken CssToken deriving (Eq)

instance Show AtRuleElem where
    show (AtRuleRule r) = show r
    show (AtRuleRuleElem r) = show r
    show (AtRuleToken t) = show t
    showList rs = mconcat $ shows <$> rs

atRuleBlock = char '{' *> skipSpace' *> parseInsideBlock <* skipSpace' <* endChar '}' <* skipSpace'
    where parseInsideBlock = parseSensible <|> parseJunk
          parseSensible = sepBy'' (AtRuleRuleElem <$> (parseJust ruleSemiColonSep)) (char ';' *> skipSpace') (((AtRuleRuleElem <$> (parseJust ruleNoSep)) <|> (AtRuleRule <$> rule)) <* skipSpace')
          parseJunk = many (AtRuleToken <$> (parseJust cssBlockElem))

newtype AtRule = AtRule (String, [CssToken], Maybe [AtRuleElem]) deriving (Eq)

instance Show AtRule where
    show (AtRule (s, ts, Just es)) = "@" ++ s ++ " " ++ show ts ++ "{\n" ++ show es ++ "}\n"
    show (AtRule (s, ts, Nothing)) = "@" ++ s ++ " " ++ show ts ++ ";\n"

atRule = (AtRule <$> ((,,) <$> (atKeyword <* skipSpace') <*> many (parseJust anyCss) <*> ((Just <$> atRuleBlock) <|> (endChar ';' *> skipSpace' *> pure Nothing))))

data StyleSheetElem = StyleCDO | StyleCDC | StyleRule Rule | StyleAtRule AtRule | StyleDollarDecl DollarDeclaration deriving (Eq)

instance Show StyleSheetElem where
    show StyleCDO = "<!--"
    show StyleCDC = "-->"
    show (StyleRule r) = show r
    show (StyleAtRule r) = show r
    show (StyleDollarDecl r) = show r ++ ";\n"
    showList es = mconcat $ shows <$> es

styleSheetElem = many (comment *> skipSpace') *>
                 ((string' "<!--" *> pure StyleCDO) <|>
                 (string' "-->" *> pure StyleCDC) <|>
                 (StyleDollarDecl <$> parseJust dollarDeclaration <* skipSpace' <* char ';' <* skipSpace') <|>
                 (StyleAtRule <$> atRule) <|>
                 (StyleRule <$> rule))


styleSheet ∷ Parser [StyleSheetElem]
styleSheet = asum <$> many ((pure <$> (styleSheetElem <* skipSpace')) <|>
                             (comment *> skipSpace' *> pure []))

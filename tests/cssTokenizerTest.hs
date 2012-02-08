{-# language UnicodeSyntax #-}
import Test.HUnit
import Control.Applicative
import CssTokenizer
import Data.Attoparsec.Text
import Data.Text hiding (length)

-- for testing
checkParse p a = parseOnly p $ pack a
checkParseFile path parser = checkParse parser <$> readFile path

testList = ZipList [0..(length results - 1)]

tests ∷ [String] → Test
tests files = TestList $ getZipList $ TestCase <$> (assertEqual <$> (showTest <$> testList) <*> (resTest <$> testList) <*> (doTest <$> testList))
    where showTest = show
          doTest n = checkParse styleSheet $ files !! n
          resTest n = Right $ results !! n

main = sequence readFiles >>= 
       \f → runTestTT (tests f)
     where readFiles = getZipList $ readFile <$> ((\x → "tests/tokenTest" ++ show x ++ ".css") <$> testList)


-- we commonly use the "p colors" rule as a test
pColorsRule a = StyleRule $ Rule ([CssIdent "p"], (\x → RuleDecl $ Declaration ("color", [x])) <$> a)
idents = (CssIdent <$>)

results = [
              [
                  pColorsRule $ idents ["green"],
                  pColorsRule $ idents ["green"],
                  pColorsRule $ idents ["red", "green"],
                  pColorsRule $ idents ["green"],
                  pColorsRule $ idents ["red", "green"],
                  pColorsRule $ idents ["green"],
                  pColorsRule $ idents ["red", "green"]
              ],

              -- Tests for unexpected end of input.
              [pColorsRule [CssString "green"]],
              [pColorsRule [CssURI "green"]],
              [pColorsRule [CssFunction "func" [CssString "green"]]],
              [pColorsRule [CssParen [CssString "green"]]],
              [pColorsRule [CssBracket [CssString "green"]]],
              [pColorsRule [CssIdent "green"]],

              -- bad strings tests
              [pColorsRule $ idents ["green", "green"]]
          ]
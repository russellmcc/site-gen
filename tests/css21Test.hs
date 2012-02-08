{-# language UnicodeSyntax #-}
import Test.HUnit
import Control.Applicative
import Css21Parser
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
          doTest n = checkParse parseCss21 $ files !! n
          resTest n = Right $ results !! n

main = sequence readFiles >>= 
       \f → runTestTT (tests f)
     where readFiles = getZipList $ readFile <$> ((\x → "tests/css21Test" ++ show x ++ ".css") <$> testList)


greenWithSel s = StyleElemRule s [Declaration ("color", [CssIdent "green"])]

results = [
 Style [ImportString "number 1" [], ImportString "number 2" ["right"], ImportString "number 3" ["right", "right"]] [],
 Style [ImportString "number 1" []] [StyleElemRule [SelIdent "p"] [Declaration ("color", [CssIdent "green"])]],
 Style [ImportString "number 1" [], ImportString "number 2" []] [],
 Style [] [
    greenWithSel [SelIdent "p"], 
    greenWithSel [SelHash "p"], 
    greenWithSel [SelClass "p"], 
    greenWithSel [SelIdent "*"], 
    greenWithSel [SelAttrib "bleh" Nothing], 
    greenWithSel [SelAttrib "bleh" $ Just (MInc, "blah")], 
    greenWithSel [SelPseudo (PseudoClass "p")],
    greenWithSel [SelPseudo (PseudoFunc "p" Nothing)],
    greenWithSel [SelPseudo (PseudoFunc "p" (Just "p"))]
  ]
 ]
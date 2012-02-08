{-# OPTIONS_GHC -F -pgmF she #-}
{-# language UnicodeSyntax #-}

import Test.HUnit
import Control.Applicative
import Control.Monad
import CssTokenizer
import CssProcess
import Data.Attoparsec.Text
import Util
import Data.Text hiding (length)
import qualified Text.Parsec as P

checkParse p a = parseOnly p $ pack a

tests ∷ [(String, String, String)] → IO Test
tests specs = return $ TestList (| TestCase (| assertEqualIO  specs |) |)
    where assertEqualIO ∷ (String, String, String) → Assertion
          assertEqualIO = (| (| assertEqual showTest resTest|) =<< doTest|)
          -- do-notation definition for above:
          -- assertEqualIO = do 
          --   fs <- doTest
          --   s <- showTest
          --   r <- resTest
          --   pure $ fs >>= \f →
          --      assertEqual s r f
          showTest ∷ (String, String, String) → String
          showTest (t, _, _) = t
          doTest ∷ (String, String, String) → IO (Either String [StyleSheetElem])
          doTest (_, t, _) = process (checkParse styleSheet t)
          resTest ∷ (String, String, String) → Either String [StyleSheetElem]
          resTest (_, _, t) = checkParse styleSheet t
          left' f (Left a) = Left $ f a
          left' f (Right b) = Right b

main = tests testList >>= runTestTT -- (a → f b) → f (a → b)

testList = 
    [
     ("swallow dollar decls", "$dollar : bleh;", ""),
     ("swallow mixin decls", "@mixin {}", ""),
     ("replace easy dollar values", "$dollar : green; h {color: $dollar;}", "h {color: green}"),
     ("dollar values are evaluated", "$dollar: green; $d2: $dollar; h { color: $d2;}", "h {color: green}"),
     ("dollars get evaled dynamically", "$d: black; $d2: $d; $d: green; h {color : $d2}", "h {color: green}"),
     ("dollars can be updated inside blocks", "$d: black; $d2: $d; h {$d: green; color: $d2}", "h {color: green}"),
     ("dollars updates inside blocks are local", "$d: black; $d2: $d; h {$d: green; color: $d2} h {color: $d2;}", "h {color: green} h {color: black}"),
     ("skips infinite loops", "$d : $d2; $d2: $d; h {color: green; color : $d;}", "h {color: green}"),
     ("basic mix-ins work", "@mixin blue {color: blue} h {@include blue;}", "h {color: blue}"),
     ("don't eat random @rules", "@atrule my blablabh kkas at rule {blahl asdf blahasdf,  blah }", "@atrule my blablabh kkas at rule {blahl asdf blahasdf,  blah }"),
     ("mix-ins are evaluated", "@mixin t {color : $color;} $color: green; h {@include t;}", "h {color: green}"),
     ("no mix-in infinite loops", "@mixin a {@include b;} @mixin b {@include a;} h {@include a; color: green}", "h {color:green}"),
     ("mix-ins work with args", "@mixin color($color) {color: $color} h {@include color(green);}", "h {color: green;}"),
     ("mix-ins can have multiple args", "@mixin color($unused, $color) {color : $color} h {@include color(black, green);}", "h {color: green;}"),
     ("@rules are processed.", "$color: green; @media {h{color:$color}}", "@media{h{color: green}}"),
     ("dollars can be defined in @rules.", "@media {$color: green; h { color: $color}}", "@media { h  { color: green}}"),
     ("dollars can shift in includes.", "@mixin blue {$color : green} $color: blue; h {@include blue; color: $color}", "h {color: green}"),
     ("at rules that include rules work.", "@mixin hgreen {h { color: green}} @media screen {@include hgreen;}", "@media screen {h {color: green}}"),
     ("constant expressions can be added.", "h {number: 3 + 2}", "h {number : 5}"),
     ("variables can be added.", "$a : 7; h {number: 6 + $a}", "h {number : 13}"),
     ("dimensions can be added together if same dimension", "h {line-width: 7em + 2em}", "h {line-width: 9em}"),
     ("dimensions can't be added together if not same dimension", "h {line-width: 7em + 2en}", "h {line-width: 7em + 2en}"),
     ("numbers can be added to dimensions on the left", "h {line-width: 2 + 7em}", "h {line-width: 9em}"),
     ("numbers can be added to dimensions on the right", "h {line-width: 7em + 2}", "h {line-width: 9em}"),
     ("multiple operators are allowed.", "h {line-width: 7em + 2 + 1}", "h {line-width: 10em}"),
     ("parens are allowed.", "h {line-width: (7em + 2) + 1}", "h {line-width: 10em}"),
     ("invalid parens are passed-through.", "h {line-width: (7em + 2 +) + 1 + 7}", "h {line-width: (7em + 2 +) + 1 + 7}"),
     ("variables can be placed in parens.", "h {$a : 2; line-width: (7em + $a) + 1}", "h {line-width: 10em}"),
     ("percentages work like numbers.", "h {number: 50% * 10}", "h {number: 5}"),
     ("complex expressions work.", "$line-height: 3; $size: 2; $min-heading-lead-top: 1.1; h {num :($line-height - (($size * (1 + $min-heading-lead-top)) % $line-height)) / $size + $min-heading-lead-top * 1em;}", "h {num : 2em}"),
     ("two elements in rule works", "h{c:c; d:d;}", "h {c:c; d:d}"),
     ("including twice works", "@mixin a {a:a} @mixin b {b:b} h{@include a; c:c; @include b;}", "h {a:a; c:c; b:b}"),
     ("including the same rule twice works", "@mixin a {a:a}h{@include a; c:c; @include a;}", "h {a:a; c:c; a:a}"),
     ("stupid width rule works", "@mixin width($cols) {width: ($cols * ($column + $gutter) - $gutter) / $font-size * 1em;} $column:48; $gutter:24; $font-size:16; h {@include width(4);}", "h {width:16.5em}")

    ]

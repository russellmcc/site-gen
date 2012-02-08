{-# language UnicodeSyntax #-}

module Util where

import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Control.Applicative
import Data.Monoid
import Data.Foldable
import Data.Text hiding (count)

-- apply <|> under an applicative
a <<|>> b = (<|>) <$> a <*> b

-- apply <$> under an applicative
a <<$>> b = (<$>) <$> pure a <*> b

-- this is the equivalent of ":" for alternatives
a <:> b = pure a <|> b
a <<:>> b = (<:>) <$> a <*> b -- lifted version

-- combine functors of bools
f <||> g = (Any <$> f) `mappend` (Any <$> g)

-- parsing utility: try multiple counts.
counts c p = asum (count <$> c <*> pure p)

-- parsing utility: same as satisfy but as a string.
satisfy' ∷ (Char → Bool) → Parser String
satisfy' c = pure <$> satisfy c

-- either it satisfies the condition or succeed with no string.
optSatisfy c = option "" (satisfy' c)

-- quick fix to allow easy packing
string' = string . pack

-- This differs from sepBy in that if we end with the separator it doesn't count as a new list
sepBy' p sep = option [] (pure <$> p) <<|>> (asum <$> many (sep *> option [] (pure <$> p)))

-- Same as above but with a slot for a second parser that doesn't need to be separated.
sepBy'' ∷ Parser p → Parser q → Parser p → Parser [p]
sepBy'' p sep r =  (asum <$> many rp) <<|>> optP <<|>> (asum <$> many (rp <|> (sep *> optP)))
    where optP = option [] (pure <$> p)
          rp = (pure <$> r) <<|>> optP

getRight (Right a) = Just a
getRight (Left _) = Nothing

{-# LANGUAGE OverloadedStrings, UnicodeSyntax  #-}
module Main where

import Prelude hiding (id, foldr, elem, sum)
import Control.Arrow ((>>>), (>>^), (^>>), (***), (&&&), arr, second, first)
import Control.Category (id)
import Data.Monoid (mempty, mconcat, mappend)
import Control.Applicative
import Data.Maybe
import Data.List hiding (foldr, elem, sum)
import Data.Foldable
import Control.Monad
import CssProcess
import CssTokenizer
import Data.Attoparsec.Text (parseOnly)
import Data.Text (pack)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Internal as LBSI
import Hakyll

main :: IO ()
main = hakyll $ do

    staticCopy

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> arr renderScore
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
    
    match "resume.md" $ do
        route $ setExtension ".html"
        compile $ pageCompiler
           >>> applyTemplateCompiler "templates/resume.html"
           >>> applyTemplateCompiler "templates/default.html"
           >>> relativizeUrlsCompiler
               
    match "about.md" $ do
        route $ setExtension ".html"
        compile $ pageCompiler
           >>> applyTemplateCompiler "templates/default.html"
           >>> relativizeUrlsCompiler

    -- Index
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> requireAllA "posts/*" (id *** arr (take 3 . reverse . chronological) >>> addPostList)
        >>> sortedPosts
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Tags
    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

    -- Render RSS feed
    match "rss.xml" $ route idRoute
    create "rss.xml" $
        requireAll_ "posts/*"
            >>> mapCompiler (arr $ copyBodyToField "description")
            >>> renderRss feedConfiguration

    -- Read templates
    match "templates/*" $ compile templateCompiler
  where
    renderTagCloud' :: Compiler (Tags String) String
    renderTagCloud' = renderTagCloud tagIdentifier 100 120

    tagIdentifier :: String -> Identifier (Page String)
    tagIdentifier = fromCapture "tags/*"

byScore ∷ [Page String] → [Page String]
byScore = sortBy score'
    where score' a b = score'' a `compare` score'' b
          score'' ∷ Page String → Int
          score'' a = sum $ maybeScore a
          maybeScore ∷ Page String → Maybe Int
          maybeScore = fmap fst . listToMaybe . reads . getField "score"

getTags = map trim . splitAll "," . getField "tags"

sortedPosts ∷ Compiler (Page String) (Page String)
sortedPosts = id &&& constA () >>> 
              setFieldA "sorted" tagList 
    where tagList = requireA "tags" $
                    arr (\(_, t) → tagsMap t) >>>
                    mapCompiler makeSortedTagList >>>
                    arr mconcat

makeSortedTagList ∷ Compiler (String, [Page String]) String
makeSortedTagList = constA mempty &&& id
            >>> arr (\(p, (t, ps)) → ((p, t), ps))
            >>> first (setFieldA "tag" id)
            >>> postList' "posts" "templates/postitemtaglist.html"
            >>> applyTemplateCompiler "templates/taglistitem.html"
            >>^ pageBody

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostListForTag = second (arr $ reverse . chronological) >>>
                    postList' "posts" "templates/postitemnotag.html"

addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = second (arr $ reverse . chronological)
    >>> postList' "posts" "templates/postitem.html"

postList' field template = setFieldA field $
        require template (\p t -> map (applyTemplate t) p)
        >>^ pageBody . mconcat

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostListForTag
        >>> arr (setField "title" tag)
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "More Facts"
    , feedDescription = "A Blog"
    , feedAuthorName  = "Russell McClellan"
    , feedRoot        = "http://www.russellmcc.com"
    }

renderScore p = setField "cScore" score p
    where score = getField "score" p

staticDirs = [ "stylesheets/*", "images/*", "resume.pdf" ]

matchAll patterns comp = sequence $ match <$> patterns <*> pure comp

staticCopy = matchAll staticDirs staticCopy'
    where staticCopy' = do 
            route $ ((matchRoute "**.scss" $ setExtension "css") `mappend` idRoute)
            compile $ byExtension getResourceLBS
                   [ (".scss", processCssCompiler)
                   ]

processCssCompiler = getResourceString >>> (unsafeCompiler doScss)
    where doScss a = LBS.pack <$> ((<$>) <$> (pure LBSI.c2w) <*> (show <$> (getRight [] <$> (process $ parseOnly styleSheet $ pack a))))
          getRight o (Left _) = o
          getRight _ (Right i) = i


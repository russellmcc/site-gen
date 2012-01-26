{-# LANGUAGE OverloadedStrings, UnicodeSyntax  #-}
module Main where

import Prelude hiding (id, foldr, elem, sum)
import Control.Arrow ((>>>), (***), (&&&), arr, second)
import Control.Category (id)
import Data.Monoid (mempty, mconcat)
import Control.Applicative
import Data.Maybe
import Data.List hiding (foldr, elem, sum)
import Data.Foldable

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

    -- Index
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> requireAllA "posts/*" (id *** arr (take 3 . reverse . chronological) >>> addPostList)
        >>> requireAllA "posts/*" sortedPosts
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

sortedPosts ∷ Compiler (Page String, [Page String]) (Page String)
sortedPosts = setFieldA "sorted" (arr . const $ "Sorted")

makeAllSorted ∷ [String] → [Page String] → [Compiler Page String String]
makeAllSorted ts posts = getZipList $ makeSortedTagList <$> ZipList ts <*> (ZipList $ postsWithTag <$> ts <*> pure posts)
    where postsWithTag t = filter (postHasTag t)
          postHasTag t p = t `elem` getTags p
          getTags = map trim . splitAll "," . getField "tags"

makeSortedTagList ∷ String → [Page String] → Compiler (Page String) String
makeSortedTagList t posts = id &&& constA (byScore posts)
            >>> postList' "posts" "template/postitemtaglist.html"
            >>> arr (setField "tag" t)
            >>> applyTemplateCompiler "template/taglistitem.html"
            >>> arr pageBody

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = second (arr $ reverse . chronological)
    >>> postList' "posts" "template/postitem.html"

postList' field template = setFieldA field $
        require template (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList
        >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
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

staticDirs = [ "stylesheets/*", "images/*" ]

matchAll patterns comp = sequence $ match <$> patterns <*> pure comp

staticCopy = matchAll staticDirs staticCopy'
    where staticCopy' = do 
            route idRoute
            compile copyFileCompiler


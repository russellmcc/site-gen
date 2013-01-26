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

postCtx ∷ Context String
postCtx = mconcat [ dateField "date" "%B %e, %Y",
                    defaultContext
                  ]

main :: IO ()
main = hakyll $ do

    staticCopy
    cssProcess

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ compileWithTemplate "templates/post.html"
    
    match "resume.md" $ do
        route $ setExtension ".html"
        compile $ compileWithTemplate "templates/resume.html"
               
    match "about.md" $ do
        route $ setExtension ".html"
        compile $ compileWithTemplate "templates/resume.html"

    createListOfPosts "index.html" "templates/index.html" (take 3 . recentFirst)
    createListOfPosts "archive.html" "templates/posts.html" recentFirst

    create ["rss.xml"] $do
      route idRoute
      compile $ do
        posts <- chronological <$> loadAllSnapshots "posts/*" "content"
        fullCtx <- pure (field "description" (pure . itemBody) `mappend` postCtx)
        renderRss feedConfiguration fullCtx posts

    match "templates/*" $ compile templateCompiler

  where
    doTpl t = applyTemplate t postCtx
    compileWithTemplate a = do
        tpl <- loadBody a
        defaultTpl <- loadBody "templates/default.html"                          
        pandocCompiler >>= doTpl tpl >>= saveSnapshot "content" >>= doTpl defaultTpl >>= relativizeUrls

    createListOfPosts ident tpl postSort = 
        create [ident] $ do
          route idRoute
          compile $ do
            posts <- postSort <$> loadAll "posts/*"
            itemTpl <- loadBody "templates/postitem.html"
            postList <- applyTemplateList itemTpl postCtx posts
            fullCtx <- pure (field "posts" (pure $ pure postList) `mappend` postCtx)
            indTpl <- loadBody tpl
            defTpl <- loadBody "templates/default.html"
            makeItem "" >>=
             applyTemplate indTpl fullCtx >>=
             applyTemplate defTpl fullCtx >>=
             relativizeUrls
                 
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "More Facts"
    , feedDescription = "A Blog"
    , feedAuthorName  = "Russell McClellan"
    , feedRoot        = "http://www.russellmcc.com"
    }


staticDirs = [ "stylesheets/*.css", "images/*", "resume-russell-mcclellan.pdf" ]

matchAll patterns comp = sequence $ match <$> patterns <*> pure comp

staticCopy = matchAll staticDirs $ do
               route idRoute
               compile copyFileCompiler

cssProcess = match "**.scss" $ do
               route $ setExtension "css"
               compile processCssCompiler

processCssCompiler = do
      a <- getResourceString
      unsafeCompiler (doScss $ itemBody a) >>= makeItem
    where doScss a = LBS.pack <$> ((<$>) <$> pure LBSI.c2w <*> (show <$> (getRight [] <$> process (parseOnly styleSheet $ pack a))))
          getRight o (Left _) = o
          getRight _ (Right i) = i


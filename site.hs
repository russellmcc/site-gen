{-# LANGUAGE OverloadedStrings, UnicodeSyntax  #-}
module Main where

import Prelude hiding (id, foldr, elem, sum)
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
import Data.Set as S
import Text.Pandoc hiding (applyTemplate)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Internal as LBSI
import Hakyll

postCtx ∷ Context String
postCtx = mconcat [ dateField "date-human" "%B %e, %Y",
                    dateField "date-8601" "%Y-%m-%d",
                    defaultContext
                  ]

myPandocCompiler ∷ Compiler (Item String)
myPandocCompiler = pandocCompilerWith myReaderOptions myWriterOptions
  where
    myWriterOptions = def
                      { writerHTMLMathMethod = MathJax ""
                      }
    myReaderOptions = def
                      {
                        readerExtensions = mconcat (enableExtension <$>
                          [ Ext_definition_lists
                          , Ext_multiline_tables
                          , Ext_markdown_in_html_blocks
                          ]) strictExtensions
                      }

main :: IO ()
main = hakyll $ do

    staticCopy
    cssProcess

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ compileWithTemplate "templates/post.html" (Just "templates/afterpost.html")


    createListOfPosts "index.html" "templates/index.html" recentFirst

    create ["rss.xml"] $do
      route idRoute
      compile $ do
        unsortedPosts <- loadAllSnapshots "posts/*" "content"
        posts <- chronological unsortedPosts
        fullCtx <- pure (field "description" (pure . itemBody) `mappend` postCtx)
        renderRss feedConfiguration fullCtx posts

    match "templates/*" $ compile templateCompiler

  where
    doTpl t = applyTemplate t postCtx
    maybeDoTpl (Just t) s = do
                  tpl <- loadBody t
                  doTpl tpl s
    maybeDoTpl Nothing s = return s
    compileWithTemplate a b = do
        tpl <- loadBody a
        defaultTpl <- loadBody "templates/default.html"
        myPandocCompiler >>=
         doTpl tpl >>=
         saveSnapshot "content" >>=
         maybeDoTpl b >>=
         doTpl defaultTpl >>=
         relativizeUrls

    createListOfPosts :: Identifier -> Identifier -> ([Item String] -> Compiler [Item String]) -> Rules ()
    createListOfPosts ident tpl postSort =
        create [ident] $ do
          route idRoute
          compile $
            makeItem "" >>=
            loadAndApplyTemplate tpl fullCtx >>=
            loadAndApplyTemplate "templates/default.html" fullCtx >>=
            relativizeUrls
        where
          sortedPostList ∷ Compiler [Item String]
          sortedPostList = (loadAll "posts/*") >>= postSort

          -- do a (reverse) bind under a monad
          (=<$<) ∷ (Monad m) ⇒ m (a → m b) → m a → m b
          f =<$< b = (b >>=) =<< f
          infixr 1 =<$<

          allPostBodies ∷ Compiler String
          allPostBodies = applyTemplateList <$>
                          loadBody "templates/postitem.html" <*>
                          pure postCtx =<$<
                          sortedPostList

          fullCtx = field "posts" (pure allPostBodies) `mappend` postCtx

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Russell McClellan"
    , feedDescription = "russellmcc.com"
    , feedAuthorName  = "Russell McClellan"
    , feedRoot        = "http://www.russellmcc.com"
    , feedAuthorEmail = "russell.mcclellan@gmail.com"
    }


staticDirs = [ "stylesheets/*.css", "images/*", "fonts/*", "CNAME" ]

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

{-# LANGUAGE OverloadedStrings, UnicodeSyntax  #-}
module Main where

import Prelude hiding (id, foldr, elem, sum)
import Control.Category (id)
import Data.Monoid (mempty, mconcat, mappend)
import Data.String (fromString)
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
import System.FilePath.Posix (takeBaseName, splitPath, joinPath)
import Hakyll
import Debug.Trace (trace)

postCtx ∷ Context String
postCtx = mconcat [ dateField "date-human" "%B %e, %Y",
                    dateField "date-8601" "%Y-%m-%d",
                    defaultContext
                  ]

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

myPandocCompiler ∷ Compiler (Item String)
myPandocCompiler = pandocCompilerWith myReaderOptions myWriterOptions

compileWithTemplate a b comp = do
  tpl <- loadBody a
  defaultTpl <- loadBody "templates/default.html"
  comp >>=
       doTpl tpl >>=
       saveSnapshot "content" >>=
       maybeDoTpl b >>=
       doTpl defaultTpl >>=
       relativizeUrls
  where
    doTpl t = applyTemplate t postCtx
    maybeDoTpl (Just t) s = do
      tpl <- loadBody t
      doTpl tpl s
    maybeDoTpl Nothing s = return s


-- TODO run build script
doGen :: String -> Compiler (Item String)
doGen s = (unsafeCompiler doGen') >> makeItem "" where
  doGen' :: IO ()
  doGen' = putStrLn $ "COMPILING:" <> s

dropNFolders n x = joinPath $ Data.List.drop n $ splitPath x

genPost :: Identifier -> Rules ()
genPost i = let
  postName = takeBaseName $ toFilePath i
  patt = (fromString $ "gen/" <> postName <> "/**") .&&. (complement $ fromString $ "gen/" <> postName <> "/build/*")
  dummyI = fromFilePath $ "dummy-gen-" <> postName
  in do
    -- TODO: refactor to have a dummy item representing the gen step
    pattD <- makePatternDependency patt
    rulesExtraDependencies [pattD, IdentifierDependency i] $ do
        create [dummyI] $ do
            compile $ doGen postName

    rulesExtraDependencies [IdentifierDependency dummyI] $ do
        create [fromString $ "gen/" <> postName <> "/build/index.html"] $ do
            compile getResourceBody
        match (fromString $ "gen/" <> postName <> "/build/static/**") $ do
            route $ customRoute (\x -> "posts/" <> postName <> "/static/" <>
                                       (dropNFolders 4 $ toFilePath x))
            compile $ copyFileCompiler

    match (Hakyll.fromList [i]) $ do
        route $ customRoute (\_ -> "posts/" <> postName <> "/index.html")
        compile $ compileWithTemplate "templates/post.html"
                  (Just "templates/afterpost.html")
                  ((load $ fromFilePath $ "gen/" <> postName <> "/build/index.html") >>= (makeItem . itemBody))

renderDescription :: Context String
renderDescription = field "description" go where
  go :: Item String -> Compiler String
  go i = maybe (pure $ itemBody i) (doRender i) =<< (lookupString "description" <$> (getMetadata $ itemIdentifier i))
  doRender :: Item String -> String -> Compiler String
  doRender i s = itemBody <$> (renderPandocWith myReaderOptions myWriterOptions $
                               Item (itemIdentifier i) s)

main :: IO ()
main = hakyll $ do
    cssProcess
    staticCopy

    postsToGen <- ((fst <$>) . (Data.List.filter (hasGen . snd))) <$> getAllMetadata "posts/*"
    sequence $ genPost <$> postsToGen

    -- Render posts
    matchMetadata "posts/*" noGen $ do
        route   $ setExtension ".html"
        compile $ compilePost

    match "templates/*" $ compile templateCompiler

    createListOfPosts "index.html" "templates/index.html" recentFirst

    create ["rss.xml"] $do
      route idRoute
      compile $ do
        unsortedPosts <- loadAllSnapshots "posts/*" "content"
        posts <- chronological unsortedPosts
        renderRss feedConfiguration (renderDescription <> postCtx) posts

  where

    hasGen m = isJust $ lookupString "gen" m
    noGen m = isNothing $ lookupString "gen" m

    compilePost = compileWithTemplate "templates/post.html" (Just "templates/afterpost.html") myPandocCompiler

    createListOfPosts :: Identifier -> Identifier -> ([Item String] -> Compiler [Item String]) -> Rules ()
    createListOfPosts ident tpl postSort =
        create [ident] $ do
          route idRoute
          compile $
            allPostBodies >>= makeItem >>=
            loadAndApplyTemplate tpl postCtx >>=
            loadAndApplyTemplate "templates/default.html" postCtx >>=
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

cssProcess = match "stylesheets/*.scss" $ do
               route $ setExtension "css"
               compile processCssCompiler

processCssCompiler = do
      a <- getResourceString
      unsafeCompiler (doScss $ itemBody a) >>= makeItem
    where doScss a = LBS.pack <$> ((<$>) <$> pure LBSI.c2w <*> (show <$> (getRight [] <$> process (parseOnly styleSheet $ pack a))))
          getRight o (Left _) = o
          getRight _ (Right i) = i

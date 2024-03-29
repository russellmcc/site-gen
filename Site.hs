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
import System.Process
import System.Directory (doesFileExist)
import qualified System.FilePath.Glob as G

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
doGen :: String -> IO ()
doGen s = (putStrLn $ "COMPILING:" <> s) >>
          (readCreateProcess ((proc "npm" ["install"]){ cwd = Just $ "gen/" <> s }) "") >>
          pure ()

dropNFolders n x = joinPath $ Data.List.drop n $ splitPath x

genPost :: Identifier -> Rules ()
genPost i = let
  postName = takeBaseName $ toFilePath i
  pattStatic = (fromString $ "gen/" <> postName <> "/static/**")
  pattNode = (fromString $ "gen/" <> postName <> "/node_modules/**")
  pattBuild = (fromString $ "gen/" <> postName <> "/build/**")
  pattGit = (fromString $ "gen/" <> postName <> "/.git/**")
  patt = (fromString $ "gen/" <> postName <> "/src/*") .&&. (complement (pattStatic .||. pattNode .||. pattBuild .||. pattGit))
  indexFile = "gen/" <> postName <> "/build/index.html"
  jsFile = "gen/" <> postName <> "/build/index.js"
  loadIndex :: Compiler (Item String)
  loadIndex = load $ fromFilePath $ indexFile
  in do
    match pattStatic $ do
        route $ customRoute (\x -> "posts/" <> postName <> "/static/" <>
                             (dropNFolders 3 $ toFilePath x))
        compile $ copyFileCompiler

    match patt $ do
        compile $ copyFileCompiler

    create [fromString $ indexFile] $ do
        compile $ getMatches patt >>= \is ->
                  sequence ((load <$> is) :: [Compiler (Item CopyFile)]) >>
                  (unsafeCompiler $ doGen postName) >>
                  (unsafeCompiler $ readFile indexFile) >>=
                  makeItem

    create [fromString jsFile] $ do
        route $ customRoute (\_ -> "posts/" <> postName <> "/index.js")
        compile $ (loadIndex) >> (unsafeCompiler (readFile jsFile)) >>= makeItem

    match (Hakyll.fromList [i]) $ do
        route $ customRoute (\_ -> "posts/" <> postName <> "/index.html")
        compile $ compileWithTemplate "templates/post-gen.html"
                  (Just "templates/afterpost.html")
                  (loadIndex >>= (makeItem . itemBody))

renderDescription :: Context String
renderDescription = field "description" go where
  go :: Item String -> Compiler String
  go i = maybe (pure $ itemBody i) (doRender i) =<< (lookupString "description" <$> (getMetadata $ itemIdentifier i))
  doRender :: Item String -> String -> Compiler String
  doRender i s = itemBody <$> (renderPandocWith myReaderOptions myWriterOptions $
                               Item (itemIdentifier i) s)



config = defaultConfiguration {
           ignoreFile = \p ->
                        (matches (fromString "**/build/**") (fromFilePath p)) ||
                        (matches (fromString "**/.idyll/**") (fromFilePath p)) ||
                        (matches (fromString ".idyll") (fromFilePath p)) ||
                        (matches (fromString "build") (fromFilePath p))
         }

main :: IO ()
main = hakyllWith config $ do
    cssProcess
    staticCopy

    postsToGen <- ((fst <$>) . (Data.List.filter (hasGen . snd))) <$> getAllMetadata "posts/*"
    sequence $ genPost <$> postsToGen

    -- Render posts
    matchMetadata "posts/*" noGen $ do
        route   $ setExtension ".html"
        compile $ compilePost

    match "templates/*" $ compile templateCompiler

    match "resume.md" $ do
        route $ setExtension ".html"
        compile $ compileWithTemplate "templates/resume.html" Nothing myPandocCompiler

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


staticDirs = [ "stylesheets/*.css", "images/*", "fonts/*", "CNAME", "resume-russell-mcclellan.pdf" ]

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

--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Walk

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith myConfiguration $ do
    match "images/*" copyAsIs
    match "slides/*.pdf" copyAsIs
    match "slides/*.hs" copyAsIs
    match "slides/*.py" copyAsIs
    match "slides/*.txt" copyAsIs
    match "slides/*.agda" copyAsIs
    match "practicals/*.pdf" copyAsIs
    match "practicals/*.hs" copyAsIs
    match "practicals/*.zip" copyAsIs
    match "exams/*.pdf" copyAsIs
    match "exams/*.hs" copyAsIs
    match "exercises/*.tar.gz" copyAsIs

    match "js/*.js" copyAsIs

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "images/*.svg" copyAsIs
    match "images/*.png" copyAsIs
    match "images/*.jpg" copyAsIs

    match ("*.md" .||. "*.org") $ do
        route   $ setExtension "html"
        compile $ pandocCompiler'
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "*.org" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler'
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "exercises/*.org" $ do
        route   $ setExtension "html"
        compile $ exerciseCompiler NoSolutions
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "exercises/*.org" $ version "solutions" $ do
        route   $ setExtension ".solutions.html"
        compile $ exerciseCompiler WithSolutions
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $
        compile templateCompiler

copyAsIs = do
    route   idRoute
    compile copyFileCompiler


-- use mathjax
defaultWriterOptons = defaultHakyllWriterOptions {
                        writerHTMLMathMethod = MathJax defaultMathJaxURL
                      }

data WithSolutions = NoSolutions | WithSolutions deriving (Show,Eq)


exerciseCompiler sols =
    pandocCompilerWithTransform defaultHakyllReaderOptions writerOptions transform
  where
    writerOptions = defaultWriterOptons { writerNumberSections = True }
    transform = case sols of
      WithSolutions -> id
      NoSolutions   -> noSolutions


-- | Our default pandoc compiler
pandocCompiler' = pandocCompilerWith defaultHakyllReaderOptions defaultWriterOptons


myConfiguration :: Configuration
myConfiguration = defaultConfiguration {
    deployCommand = "rsync -avh _site/* gemini.science.uu.nl:/science/wwwprojects/ics/www/docs/vakken/fp/2024"
  }




--------------------------------------------------------------------------------

noSolutions :: Pandoc -> Pandoc
noSolutions = walk (filter noSolutions')
  where
    noSolutions'   :: Block -> Bool
    noSolutions' b = case b of
      CodeBlock attr _ -> not $ hasAttribute "solution" attr
      _                -> True

    hasAttribute needle (_id, _classes, kvs) = case lookup needle kvs of
                                                 Just "yes" -> True
                                                 _          -> False

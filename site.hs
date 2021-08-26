--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Text.Pandoc.Options

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

    -- Password-protected
    {-
    match "pw/*.html" copyAsIs
    match "pw/htaccess" $ do
        route   (constRoute "pw/.htaccess")
        compile copyFileCompiler
    match "pw/htpasswd" $ do
        route   (constRoute "pw/.htpasswd")
        compile copyFileCompiler
    -}

    match "js/*.js" copyAsIs

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "images/*.svg" copyAsIs
    match "images/*.png" copyAsIs
    match "images/*.jpg" copyAsIs

    match "*.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler'
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match ("*.org" .||. "exercises/*.org") $ do
        route   $ setExtension "html"
        compile $ pandocCompiler'
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls


    match "templates/*" $
        compile templateCompiler

copyAsIs = do
    route   idRoute
    compile copyFileCompiler


pandocCompiler' = pandocCompilerWith defaultHakyllReaderOptions writerOptions
  where
    -- use mathjax
    writerOptions = defaultHakyllWriterOptions {
                      writerHTMLMathMethod = MathJax defaultMathJaxURL
                      }



myConfiguration :: Configuration
myConfiguration = defaultConfiguration {
    deployCommand = "rsync -avh _site/* gemini.science.uu.nl:/science/wwwprojects/cs-www/wwwcs/docs/vakken/fp/2021"
  }

--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" copyAsIs
    match "slides/*.pdf" copyAsIs
    match "practicals/*.pdf" copyAsIs
    match "practicals/*.hs" copyAsIs
    
    match "css/*" $ do 
        route   idRoute
        compile compressCssCompiler

    match "*.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

copyAsIs = do 
    route   idRoute
    compile copyFileCompiler
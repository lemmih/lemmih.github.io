--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Data.ByteString.Base64.Lazy
import           Data.ByteString.Lazy.Char8  (unpack)
import           Data.Monoid                 (mappend)
import           Hakyll
import Data.Maybe
import System.FilePath

import Debug.Trace

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*.jpg" $ do
      route    $ setExtension "html"
      compile $ do
        lbs <- getResourceLBS
        let base64 = fmap (unpack . encode) lbs
        let pageCtx =
                field "recent_posts" (\_ -> recentPostList) `mappend`
                constField "title" siteTitle            `mappend`
                postCtx
        pure base64
          >>= loadAndApplyTemplate "templates/drawing.html"    postCtx
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" pageCtx
          >>= relativizeUrls

    match "posts/*.markdown" $ do
        route   $ setExtension "html"
        compile $ do
            let pageCtx =
                    field "recent_posts" (\_ -> recentPostList) `mappend`
                    constField "title" siteTitle            `mappend`
                    postCtx


            pandocCompiler
              >>= loadAndApplyTemplate "templates/post.html"    postCtx
              >>= saveSnapshot "content"
              >>= loadAndApplyTemplate "templates/default.html" pageCtx
              >>= relativizeUrls

    match (fromList ["about.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ do
            let pagesCtx =
                    field "recent_posts" (\_ -> recentPostList) `mappend`
                    constField "title" siteTitle            `mappend`
                    constField "site_desc" siteDesc          `mappend`
                    defaultContext

            pandocCompiler
                >>= loadAndApplyTemplate "templates/page.html" defaultContext
                >>= loadAndApplyTemplate "templates/default.html" pagesCtx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    field "recent_posts" (\_ -> recentPostList) `mappend`
                    constField "title" "Archives"            `mappend`
                    constField "site_desc" siteDesc          `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let indexCtx =
                    field "recent_posts" (\_ -> recentPostList) `mappend`
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" siteTitle         `mappend`
                    constField "site_desc" siteDesc          `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
-- Metadata
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    constField "site_desc" siteDesc `mappend`
    field "previousPostUrl" (previousPostUrl "posts/*") <>
    field "previousPostTitle" (previousPostTitle "posts/*") <>
    field "nextPostUrl" (nextPostUrl "posts/*") <>
    field "nextPostTitle" (nextPostTitle "posts/*") <>
    defaultContext

siteTitle :: String
siteTitle = "Drawings"

siteDesc :: String
siteDesc = "Lemmih's journey into drawing"

--------------------------------------------------------------------------------
-- Recent Posts
recentPosts :: Compiler [Item String]
recentPosts = do
    identifiers <- getMatches "posts/*"
    return [Item identifier "" | identifier <- identifiers]

recentPostList :: Compiler String
recentPostList = do
    posts   <- fmap (take 10) . recentFirst =<< recentPosts
    itemTpl <- loadBody "templates/listitem.html"
    list    <- applyTemplateList itemTpl defaultContext posts
    return list



withRelatedPost:: (Show a, Show t, MonadMetadata m, Alternative m) =>
    (Identifier -> [Identifier] -> Maybe t) -> (t -> m b) -> Pattern -> Item a -> m b
withRelatedPost r f pattern item = do
    idents <- getMatches pattern >>= sortRecentFirst
    let id = itemIdentifier item
        prevId = r id idents
    case prevId of
        Just i -> trace ("Found: " ++ show (itemIdentifier item) ++ " -> " ++ show i) $ f i
        Nothing -> trace ("Empty: " ++ show (itemIdentifier item)) $ empty

withPreviousPost :: (Show a) => (Identifier -> Compiler b) -> Pattern -> Item a -> Compiler b
withPreviousPost = withRelatedPost itemAfter
    where
        itemAfter x xs = lookup x $ zip xs (tail xs)

withNextPost :: (Show a) => (Identifier -> Compiler b) -> Pattern -> Item a -> Compiler b
withNextPost = withRelatedPost itemBefore
    where
        itemBefore x xs = lookup x $ zip (tail xs) xs

previousPostUrl :: Pattern -> Item String -> Compiler String
previousPostUrl = withPreviousPost (fmap (maybe empty toUrl) . getRoute)

previousPostTitle :: Pattern -> Item String -> Compiler String
previousPostTitle = withPreviousPost (\i -> fmap (fromMaybe (takeBaseName $ toFilePath i)) (getMetadataField i "title"))

nextPostUrl :: Pattern -> Item String -> Compiler String
nextPostUrl = withNextPost (fmap (maybe empty toUrl) . getRoute)

nextPostTitle :: Pattern -> Item String -> Compiler String
nextPostTitle = withNextPost (\i -> fmap (fromMaybe (takeBaseName $ toFilePath i)) (getMetadataField i "title"))

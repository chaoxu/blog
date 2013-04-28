-- | This module provides means for reading and applying 'Template's.
--
-- Templates are tools to convert items into a string. They are perfectly suited
-- for laying out your site.
--
-- Let's look at an example template:
--
-- > <html>
-- >     <head>
-- >         <title>My crazy homepage - $title$</title>
-- >     </head>
-- >     <body>
-- >         <div id="header">
-- >             <h1>My crazy homepage - $title$</h1>
-- >         </div>
-- >         <div id="content">
-- >             $body$
-- >         </div>
-- >         <div id="footer">
-- >             By reading this you agree that I now own your soul
-- >         </div>
-- >     </body>
-- > </html>
--
-- As you can see, the format is very simple -- @$key$@ is used to render the
-- @$key$@ field from the page, everything else is literally copied. If you want
-- to literally insert @\"$key$\"@ into your page (for example, when you're
-- writing a Hakyll tutorial) you can use
--
-- > <p>
-- >     A literal $$key$$.
-- > </p>
--
-- Because of it's simplicity, these templates can be used for more than HTML:
-- you could make, for example, CSS or JS templates as well.
module Template
    ( loadAndApplyTemplate
    ) where

-- | Module containing the template data structure
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Web.Template.Internal
    ( Template (..)
    , TemplateElement (..)
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative  ((<$>))
import           Data.Binary          (Binary, get, getWord8, put, putWord8)
import           Data.Typeable        (Typeable)


--------------------------------------------------------------------------------
import           Hakyll.Core.Writable


--------------------------------------------------------------------------------
import           Control.Monad                (forM, liftM)
import           Data.Monoid                  (mappend)
import           Prelude                      hiding (id)
import           Data.List                    (isPrefixOf)

--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Web.Template.Context


--------------------------------------------------------------------------------
-- | Read a template.
templateCompiler :: Compiler (Item Template)
templateCompiler = cached "Hakyll.Web.Template.templateCompiler" $ do
    item <- getResourceString
    return $ fmap readTemplate item


--------------------------------------------------------------------------------
applyTemplate :: Template                -- ^ Template
              -> Context a               -- ^ Context
              -> Item a                  -- ^ Page
              -> Compiler (Item String)  -- ^ Resulting item
applyTemplate tpl context item = do
    -- Appending missingField gives better error messages
    let context' k x = unContext (context `mappend` missingField) k x
    body <- applyTemplateWith context' tpl item
    return $ itemSetBody body item


--------------------------------------------------------------------------------
-- | The following pattern is so common:
--
-- > tpl <- loadBody "templates/foo.html"
-- > someCompiler
-- >     >>= applyTemplate tpl context
--
-- That we have a single function which does this:
--
-- > someCompiler
-- >     >>= loadAndApplyTemplate "templates/foo.html" context
loadAndApplyTemplate :: Identifier              -- ^ Template identifier
                     -> Context a               -- ^ Context
                     -> Item a                  -- ^ Page
                     -> Compiler (Item String)  -- ^ Resulting item
loadAndApplyTemplate identifier context item = do
    tpl <- loadBody identifier
    applyTemplate tpl context item


--------------------------------------------------------------------------------
-- | It is also possible that you want to substitute @$key$@s within the body of
-- an item. This function does that by interpreting the item body as a template,
-- and then applying it to itself.
applyAsTemplate :: Context String          -- ^ Context
                -> Item String             -- ^ Item and template
                -> Compiler (Item String)  -- ^ Resulting item
applyAsTemplate context item =
    let tpl = readTemplate $ itemBody item
    in applyTemplate tpl context item


--------------------------------------------------------------------------------
-- | Overloaded apply template function to work in an arbitrary Monad.
applyTemplateWith :: Monad m
                  => (String -> a -> m String)
                  -> Template -> a -> m String
applyTemplateWith context tpl x = liftM concat $
    forM (unTemplate tpl) $ \e -> case e of
        Chunk c -> return c
        Escaped -> return "♢"
        Key k   -> context k x


--------------------readTemplate

-- | Construct a @Template@ from a string.
readTemplate :: String -> Template
readTemplate = Template . readTemplate'
  where
    readTemplate' [] = []
    readTemplate' string
        | "♢♢" `isPrefixOf` string =
            Escaped : readTemplate' (drop 2 string)
        | "♢" `isPrefixOf` string =
            case readKey (drop 1 string) of
                Just (key, rest) -> Key key : readTemplate' rest
                Nothing          -> Chunk "♢" : readTemplate' (drop 1 string)
        | otherwise =
            let (chunk, rest) = break (== '♢') string
            in Chunk chunk : readTemplate' rest

    -- Parse an key into (key, rest) if it's valid, and return
    -- Nothing otherwise
    readKey string =
        let (key, rest) = span validKeyChar string
        in if not (null key) && "♢" `isPrefixOf` rest
            then Just (key, drop 1 rest)
            else Nothing

    validKeyChar x = x `notElem` ['♢', '\n', '\r']

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Datatype used for template substitutions.
newtype Template = Template
    { unTemplate :: [TemplateElement]
    }
    deriving (Show, Eq, Binary, Typeable)


--------------------------------------------------------------------------------
instance Writable Template where
    -- Writing a template is impossible
    write _ _ = return ()


--------------------------------------------------------------------------------
-- | Elements of a template.
data TemplateElement
    = Chunk String
    | Key String
    | Escaped
    deriving (Show, Eq, Typeable)


--------------------------------------------------------------------------------
instance Binary TemplateElement where
    put (Chunk string)    = putWord8 0 >> put string
    put (Key key)  = putWord8 1 >> put key
    put (Escaped) = putWord8 2

    get = getWord8 >>= \tag -> case tag of
            0 -> Chunk <$> get
            1 -> Key   <$> get
            2 -> return Escaped
            _ -> error $  "Hakyll.Web.Template.Internal: "
                       ++ "Error reading cached template"


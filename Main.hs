{-# LANGUAGE OverloadedStrings #-}

-- This example requires manual rebuild (as opposed to dynamic ones, automatically rebuilding the
-- config upon changes). This config is useful for distribution of the editor in binary form as such
-- a build have almost all libraries statically linked in.
-- Here's a building example with "stack":
-- 1. Edit "stack.yaml" file so that "location: " would point to the root of the Yi source code
-- 2. Run "stack install"
-- The final name of the executable can be changed in the "package.yaml" file.

import Control.Monad.State.Lazy (execStateT)
import Data.List                (intersperse)
import Control.Lens hiding (argument)
import Data.Maybe               (fromMaybe)
import Data.Monoid              ((<>))
import Control.Monad (void)

import Options.Applicative

import Yi hiding (option)
import Yi.Config.Simple.Types

import Yi.Buffer.Misc (lineMoveRel)
import Yi.Config.Default.HaskellMode    (configureHaskellMode)
import Yi.Config.Default.JavaScriptMode (configureJavaScriptMode)
import Yi.Config.Default.MiscModes      (configureMiscModes)

import Yi.Config.Default.Vim (configureVim)
import Yi.Config.Default.Vty (configureVty)
--import Yi.Config.Default.Emacs (configureEmacs)
import Yi.Config.Default.Pango (configurePango)
import Yi.Config.Simple (fontName, fontSize, theme)

import Yi.Intero
import qualified Data.Attoparsec.Text as P
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import Data.Text (Text,unpack)

import qualified Yi.Keymap.Vim as V
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Keymap.Vim.Ex.Types as V
import qualified Yi.Keymap.Vim.Ex.Commands.Common as V
import qualified Yi.Keymap.Vim.Utils as V

import Yi.Mode.Haskell as Hs (fastMode, cleverMode, preciseMode)
import Yi.Config.Simple (addMode)

import Themes

frontends :: [(String, ConfigM ())]
frontends = [
  ("pango", configurePango),
  ("vty", configureVty),
  ("", pure ())
  ]

keymaps :: [(String, ConfigM ())]
keymaps = [
  --("emacs", configureEmacs),
  ("vim", configureVim),
  ("", pure ())
  ]

data CommandLineOptions = CommandLineOptions {
    frontend :: Maybe String
  , keymap :: Maybe String
  , startOnLine :: Maybe Int
  --, theme :: Maybe Theme
  , files :: [String]
  }

config :: Config
config = defaultConfig 
  & fontName .~ Just "Iosevka"
  & fontSize .~ Just 11
  & theme    .~ solarizedDark

-- stolen from ethercrow/yi-config

myKeymapSet :: KeymapSet
myKeymapSet = V.mkKeymapSet $ V.defVimConfig `override` \super this ->
  let eval = V.pureEval this
  in super
  { V.vimExCommandParsers = interoExCommands <> V.vimExCommandParsers super
  , V.vimBindings = myBindings eval ++ V.vimBindings super
  }

myBindings :: (V.EventString -> EditorM ()) -> [V.VimBinding]
myBindings eval =
    let nmap x y = V.mkStringBindingE V.Normal V.Drop (x, y, id)
        nmapY x y = V.mkStringBindingY V.Normal (x, y, id)
        imapY x y = V.VimBindingY (\evs state -> case V.vsMode state of
                                    V.Insert _ ->
                                        fmap (const (y >> return V.Drop))
                                             (evs `V.matchesString` x)
                                    _ -> V.NoMatch)
        defEval = V.pureEval (extractValue V.defVimConfig)
    in [ nmap "<BS>" previousTabE
       , nmap "<Tab>" nextTabE
       , nmap " ;" (eval ":nohlsearch<CR>")
       , nmap ";" (eval ":")
       ]

-- Intero

interoExCommands :: [V.EventString -> Maybe V.ExCommand]
interoExCommands = [exInteroUses,exInteroTypeAt,exInteroEval,exInteroLocAt,exInteroStart]

exInteroEval :: V.EventString -> Maybe V.ExCommand
exInteroEval = Common.parse $ do
  void $ P.string "intero-eval"
  void $ P.many1 P.space
  instr <- P.takeWhile (const True)
  return $ Common.impureExCommand
    { V.cmdShow = "intero-eval " <> instr
    , V.cmdAction = interoEval (unpack instr)
    }

parseText :: Text -> Action -> V.EventString -> Maybe V.ExCommand
parseText txt action = Common.parse $ do
  void $ P.string txt
  return $ Common.impureExCommand
    { V.cmdShow = txt
    , V.cmdAction = action
    }

exInteroStart, exInteroLocAt,exInteroUses,exInteroTypeAt
  :: V.EventString -> Maybe V.ExCommand
exInteroStart  = parseText "intero-start"   interoStart
exInteroLocAt  = parseText "intero-loc-at"  interoLocAt
exInteroUses   = parseText "intero-uses"    interoUses
exInteroTypeAt = parseText "intero-type-at" interoTypeAt

setupConfig :: CommandLineOptions -> ConfigM ()
setupConfig opts = do
  -- Lookup f in the frontends list or pick the first element of the frontends list if
  -- f is nothing or do nothing if f is not found in the frontends list.
  let f = frontend opts
      k = keymap opts
  case f of
    Nothing -> snd (head frontends)
    Just f' -> fromMaybe (pure ()) (lookup f' frontends)
  -- Same as above, but then with k and keymaps
  case k of
    Nothing -> snd (head keymaps)
    Just k' -> fromMaybe (pure ()) (lookup k' keymaps)
  
  configureHaskellMode
  configureJavaScriptMode
  configureMiscModes
  addMode Hs.preciseMode
  defaultKmA .= myKeymapSet

commandLineOptions :: Parser (Maybe CommandLineOptions)
commandLineOptions = flag' Nothing
                       ( long "version"
                      <> short 'v'
                      <> help "Show the version number")
  <|> (Just <$> (CommandLineOptions
    <$> optional (strOption
        ( long "frontend"
       <> short 'f'
       <> metavar "FRONTEND"
       <> help "The frontend to use (default is pango)"))
    <*> optional (strOption
        ( long "keymap"
       <> short 'k'
       <> metavar "KEYMAP"
       <> help "The keymap to use (default is vim)"))
    <*> optional (option auto
        ( long "line"
       <> short 'l'
       <> metavar "NUM"
       <> help "Open the (last) file on line NUM"))
    <*> many (argument str (metavar "FILES..."))
  ))


main :: IO ()
main = do
    mayClo <- execParser opts
    case mayClo of
      Nothing -> putStrLn "Yi 0.14.0"
      Just opts -> do
        let openFileActions = intersperse (EditorA newTabE) (map (YiA . openNewFile) (files opts))
            moveLineAction  = YiA $ withCurrentBuffer (lineMoveRel (fromMaybe 0 (startOnLine opts)))
        cfg <- execStateT
            (runConfigM (setupConfig opts >> (startActionsA .= (openFileActions ++ [moveLineAction]))))
            config
        startEditor cfg Nothing
  where
   opts = info (helper <*> commandLineOptions)
     ( fullDesc
    <> progDesc "Edit files"
    <> header "Yi - a flexible and extensible text editor written in Haskell")


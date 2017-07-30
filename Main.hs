-- This example requires manual rebuild (as opposed to dynamic ones, automatically rebuilding the
-- config upon changes). This config is useful for distribution of the editor in binary form as such
-- a build have almost all libraries statically linked in.
-- Here's a building example with "stack":
-- 1. Edit "stack.yaml" file so that "location: " would point to the root of the Yi source code
-- 2. Run "stack install"
-- The final name of the executable can be changed in the "package.yaml" file.

import Control.Monad.State.Lazy (execStateT)
import Data.List                (intersperse)
import Lens.Micro.Platform      
import Data.Maybe               (fromMaybe)
import Data.Monoid              ((<>))

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
  & fontSize .~ Just 12
  & theme    .~ solarizedDark

setupConfig :: CommandLineOptions -> ConfigM ()
setupConfig clo = do
  -- Lookup f in the frontends list or pick the first element of the frontends list if
  -- f is nothing or do nothing if f is not found in the frontends list.
  let f = frontend clo
      k = keymap clo
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
      Just clo -> do
        let openFileActions = intersperse (EditorA newTabE) (map (YiA . openNewFile) (files clo))
            moveLineAction  = YiA $ withCurrentBuffer (lineMoveRel (fromMaybe 0 (startOnLine clo)))
        cfg <- execStateT
            (runConfigM (setupConfig clo >> (startActionsA .= (openFileActions ++ [moveLineAction]))))
            config
        startEditor cfg Nothing
  where
   opts = info (helper <*> commandLineOptions)
     ( fullDesc
    <> progDesc "Edit files"
    <> header "Yi - a flexible and extensible text editor written in Haskell")


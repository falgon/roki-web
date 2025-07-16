module Rules.PageType (
    PageConf (..)
  , PageConfReader
) where

import           Control.Monad.Reader (ReaderT (..))
import           Text.Pandoc.Options  (WriterOptions)
import qualified Vendor.FontAwesome   as FA
import           Vendor.KaTeX         (KaTeXRender)


data PageConf = PageConf {
    pcWriterOpt   :: WriterOptions
  , pcKaTeXRender :: KaTeXRender
  , pcFaIcons     :: FA.FontAwesomeIcons
  , pcIsPreview   :: Bool
  }

type PageConfReader = ReaderT PageConf


module Rules.Blog.Footer (
    appendFooter
) where

import           Control.Monad.Except (MonadError (..))
import           Data.Binary          (Binary)
import           Data.Time.Format     (TimeLocale, formatTime)
import           Data.Time.LocalTime  (TimeZone, utcToLocalTime)
import           Data.Typeable        (Typeable)
import           Hakyll

appendFooter :: (Binary a, Typeable a, Semigroup a)
    => String
    -> TimeLocale
    -> TimeZone
    -> Item a
    -> Compiler (Item a)
appendFooter blogName locale zone item = do
    utc <- fmap Just (getItemUTC locale (itemIdentifier item))
        `catchError` const (return Nothing)
    appendFooterWith (fmap (formatTime locale "%Y" . utcToLocalTime zone) utc) item
    where
        appendFooterWith y item' = do
            footer <- loadBody $ setVersion y $ fromFilePath (blogName <> "-footer.html")
            withItemBody (return . (<> footer)) item'

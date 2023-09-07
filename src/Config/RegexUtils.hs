{-# LANGUAGE OverloadedStrings #-}
module Config.RegexUtils (
    yyyy,
    mm,
    dd
) where

-- NOTE:
--  Hakyll uses the regex-tdfa library, which supports POSIX extended regular expressions
--  Ref. https://github.com/jaspervdj/hakyll/issues/524#issuecomment-282253949

-- Years from 1000 to 2999
{-# INLINE yyyy #-}
yyyy :: FilePath
yyyy = "[12][0-9]{3}"

{-# INLINE mm #-}
mm :: FilePath
mm = "(0?[1-9]|1[012])"

{-# INLINE dd #-}
dd :: FilePath
dd = "(0?[1-9]|[12][0-9]|3[01])"

{-# LANGUAGE OverloadedStrings #-}

module ArTimestampWiper where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Char
import           System.IO


-- | Removes the time stamps of all files in the .a file.
arFileWipeTimeStamps :: FilePath -> IO ()
arFileWipeTimeStamps path = withBinaryFile path ReadWriteMode $ \h -> do

  -- We iterate through the archive stepping from one file header to the next,
  -- setting the time stamp field to zero.
  -- The size field tells us where the next header is.
  -- See: http://en.wikipedia.org/wiki/Ar_%28Unix%29.

  archiveSize <- hFileSize h

  let go entryOffset | entryOffset == archiveSize = return () -- done, at end
                     | entryOffset >  archiveSize = die "Archive truncated"
                     -- Headers are aligned to even bytes
                     | odd entryOffset            = go (entryOffset + 1)
                     | otherwise = do

        -- Sanity check
        magic <- goto 58 >> BS.hGet h 2
        when (magic /= "\x60\x0a") $ die "Bad ar magic"

        -- Get size (to find following file)
        size <- goto 48 >> parseSize . BS8.unpack <$> BS.hGet h 10

        -- Wipe time stamp
        goto 16 >> BS.hPut h "0           " -- 12 chars

        -- Seek to next file at header + file size
        go (entryOffset + 60 + size)

        where
          goto pos = hSeek h AbsoluteSeek (entryOffset + pos)

          parseSize x = case reads x of
            [(s, r)] | all isSpace r -> s
            _                        -> die "Malformed header"

          die msg = error $ "arFileWipeTimeStamps: " ++ path ++ ": "
                            ++ msg ++ " at offset " ++ show entryOffset

  go 8 -- 8 == size of global header, before first file header



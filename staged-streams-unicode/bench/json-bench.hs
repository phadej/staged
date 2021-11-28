{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, env, nf, whnf)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE

import Unicode.JSON (unescapeText)

import qualified UnescapePure

unescapeText_staged :: BS.ByteString -> Maybe T.Text
unescapeText_staged = either (const Nothing) Just . unescapeText

unescapeText_aeson :: BS.ByteString -> Maybe T.Text
unescapeText_aeson = either (const Nothing) Just . UnescapePure.unescapeText

main :: IO ()
main = defaultMain [ escapeBench ]

escapeBench :: Benchmark
escapeBench = bgroup "Escape"
    [ example "ascii" $ BS8.pack $ take 500 $ cycle ['a'..'z']
    , example "cyrillic" $ TE.encodeUtf8 $ T.unwords
      [ "Стандарт состоит из двух основных частей: универсального набора "
      , "символов (англ. Universal character set, UCS) и семейства кодировок"
      , "(англ. Unicode transformation format, UTF). Универсальный набор"
      , "символов перечисляет допустимые по стандарту Юникод символы и"
      , "присваивает каждому символу код в виде неотрицательного целого"
      , "числа, записываемого обычно в шестнадцатеричной форме с префиксом"
      , "U+, например, U+040F. Семейство кодировок определяет способы"
      , "преобразования кодов символов для передачи в потоке или в файле."
      ]
    ]
  where
    example :: String -> BS.ByteString -> Benchmark
    example name input = bgroup name
      [ bench "staged" $ nf unescapeText_staged input
      , bench "aeson"  $ nf unescapeText_aeson input
      ]

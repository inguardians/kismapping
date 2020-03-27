module Kismapping.Input.KismetSQLite
  (
  ) where

-- https://www.stackage.org/lts-8.20/package/direct-sqlite-2.3.19


{-SELECT * FROM devices WHERE type = "Wi-Fi AP"-}

{-SELECT lat, lon, alt, signal FROM packets WHERE sourcemac = "48:5D:36:60:C1:4A"-}

{-or, if we want to do the filtering ourselves.-}

{-SELECT lat, lon, alt, signal, sourcemac FROM packets-}

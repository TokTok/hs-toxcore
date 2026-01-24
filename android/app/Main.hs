module Main (main) where

import           Foreign.Ptr       (Ptr)
import           Hatter            (startMobileApp)
import           Hatter.AppContext (AppContext)

import           Tox.Android.App   (app)

main :: IO (Ptr AppContext)
main = app >>= startMobileApp

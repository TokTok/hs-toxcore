module Main (main) where

import           Test.Hspec

import qualified Tox.Android.EmojiSpec
import qualified Tox.Android.ExportSpec
import qualified Tox.Android.FilesSpec
import qualified Tox.Android.FormatsSpec
import qualified Tox.Android.IdenticonSpec
import qualified Tox.Android.LinkedSpec
import qualified Tox.Android.LinksSpec
import qualified Tox.Android.LocationsSpec
import qualified Tox.Android.MessagesSpec
import qualified Tox.Android.ModelSpec
import qualified Tox.Android.NodesSpec
import qualified Tox.Android.QoiSpec
import qualified Tox.Android.RelaysSpec
import qualified Tox.Android.StoreSpec
import qualified Tox.Android.ViewSpec

main :: IO ()
main = hspec $ do
  describe "Emoji" Tox.Android.EmojiSpec.spec
  describe "Export" Tox.Android.ExportSpec.spec
  describe "Files" Tox.Android.FilesSpec.spec
  describe "Formats" Tox.Android.FormatsSpec.spec
  describe "Identicon" Tox.Android.IdenticonSpec.spec
  describe "Linked" Tox.Android.LinkedSpec.spec
  describe "Links" Tox.Android.LinksSpec.spec
  describe "Locations" Tox.Android.LocationsSpec.spec
  describe "Messages" Tox.Android.MessagesSpec.spec
  describe "Nodes" Tox.Android.NodesSpec.spec
  describe "Qoi" Tox.Android.QoiSpec.spec
  describe "Relays" Tox.Android.RelaysSpec.spec
  describe "Model" Tox.Android.ModelSpec.spec
  describe "Store" Tox.Android.StoreSpec.spec
  describe "View" Tox.Android.ViewSpec.spec

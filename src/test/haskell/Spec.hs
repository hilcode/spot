import Test.Hspec

import qualified Hilcode.CharSetSpec
import qualified Hilcode.ExpandedPathSpec
import qualified Hilcode.FilePathTypeSpec
import qualified Hilcode.GlobPartSpec
import qualified Hilcode.GlobPartsSpec
import qualified Hilcode.GlobSpec
import qualified Hilcode.MiscSpec
import qualified Hilcode.PathComponentGlobSpec
import qualified Hilcode.PathComponentGlobsSpec
import qualified Hilcode.QueueSpec
import qualified Hilcode.SimpleGlobSpec
import qualified Hilcode.ThreadSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "CharSet"            Hilcode.CharSetSpec.spec
    describe "ExpandedPath"       Hilcode.ExpandedPathSpec.spec
    describe "FilePathType"       Hilcode.FilePathTypeSpec.spec
    describe "Glob"               Hilcode.GlobSpec.spec
    describe "GlobPart"           Hilcode.GlobPartSpec.spec
    describe "GlobParts"          Hilcode.GlobPartsSpec.spec
    describe "Misc"               Hilcode.MiscSpec.spec
    describe "PathComponentGlob"  Hilcode.PathComponentGlobSpec.spec
    describe "PathComponentGlobs" Hilcode.PathComponentGlobsSpec.spec
    describe "Queue"              Hilcode.QueueSpec.spec
    describe "SimpleGlob"         Hilcode.SimpleGlobSpec.spec
    describe "Thread"             Hilcode.ThreadSpec.spec

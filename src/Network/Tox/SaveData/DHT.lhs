\subsection{DHT (0x02)}

\begin{code}
module Network.Tox.SaveData.DHT (DHT) where

import           Control.Arrow              (second)
import           Control.Monad              (when)
import           Data.Binary                (Binary (..))
import           Data.Binary.Get            (Get)
import qualified Data.Binary.Get            as Get
import           Data.Binary.Put            (Put)
import qualified Data.Binary.Put            as Put
import qualified Data.ByteString.Lazy       as LBS
import           Data.Word                  (Word16, Word32)
import           Network.Tox.SaveData.Nodes (Nodes)
import qualified Network.Tox.SaveData.Util  as Util
import           Test.QuickCheck.Arbitrary  (Arbitrary, arbitrary)
import qualified Test.QuickCheck.Gen        as Gen

\end{code}

This section contains a list of DHT-related sections.

\begin{tabular}{l|l}
  Length        & Contents \\
  \hline
  \texttt{4}    & \texttt{uint32\_t} (0x159000D) \\
  \texttt{?}    & List of DHT sections \\
\end{tabular}

\subsubsection{DHT Sections}

Every DHT section has the following structure:

\begin{tabular}{l|l}
  Length        & Contents \\
  \hline
  \texttt{4}    & \texttt{uint32\_t} Length of this section \\
  \texttt{2}    & \texttt{uint16\_t} DHT section type \\
  \texttt{2}    & \texttt{uint16\_t} (0x11CE) \\
  \texttt{?}    & DHT section \\
\end{tabular}

DHT section types:

\begin{tabular}{l|l}
  Name  & Value \\
  \hline
  Nodes & 0x04 \\
\end{tabular}

\paragraph{Nodes (0x04)}

This section contains a list of nodes. These nodes are used to quickly reconnect
to the DHT after a Tox client is restarted.

\begin{tabular}{l|l}
  Length        & Contents \\
  \hline
  \texttt{?}    & List of nodes \\
\end{tabular}

The structure of a node is the same as \texttt{Node Info}. Note: this means
that the integers stored in these nodes are stored in Big Endian as well.

\begin{code}

dhtMagic :: Word32
dhtMagic = 0x0159000D

sectionMagic :: Word16
sectionMagic =  0x11CE

newtype DHT = DHT [DhtSection]
    deriving (Eq, Show, Read)

instance Arbitrary DHT where
    arbitrary = DHT <$> arbitrary

instance Binary DHT where
    get = do
        magic <- Get.getWord32le
        when (magic /= dhtMagic) $
            fail $ "wrong magic number for DHT savedata: "
                ++ show magic ++ " != " ++ show dhtMagic

        DHT <$> Util.getList

    put (DHT sections) = do
        Put.putWord32le dhtMagic
        mapM_ put sections


newtype DhtSection
    = DhtSectionNodes Nodes
    deriving (Eq, Show, Read)

instance Binary DhtSection where
    get = do
        (len, ty) <- Util.getSectionHeader sectionMagic
        Get.isolate len $ case ty of
            0x04 -> DhtSectionNodes <$> get
            _    -> fail $ show ty

    put section = do
        let (ty, bytes) = second Put.runPut output

        Util.putSectionHeader sectionMagic (fromIntegral $ LBS.length bytes) ty
        Put.putLazyByteString bytes

      where
        output = case section of
            DhtSectionNodes x -> (0x04, put x)

instance Arbitrary DhtSection where
    arbitrary = Gen.oneof
        [ DhtSectionNodes <$> arbitrary
        ]

\end{code}

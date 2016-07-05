\section{Port Number}

A Port Number is a 16 bit number.  Its binary representation is a Big Endian 16
bit unsigned integer (2 bytes).

\begin{code}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy                #-}
module Network.Tox.NodeInfo.PortNumber where

import           Control.Applicative       ((<$>))
import           Data.Binary               (Binary)
import           Data.MessagePack.Class    (MessagePack)
import           Data.Word                 (Word16)
import           GHC.Generics              (Generic)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}


newtype PortNumber = PortNumber Word16
  deriving (Eq, Show, Read, Generic, Binary, Num, MessagePack)


{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


instance Arbitrary PortNumber where
  arbitrary =
    PortNumber . fromInteger <$> arbitrary
\end{code}

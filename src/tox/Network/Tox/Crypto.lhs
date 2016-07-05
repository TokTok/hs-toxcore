\chapter{Crypto}

\begin{code}
{-# LANGUAGE Safe #-}
module Network.Tox.Crypto where
\end{code}

The Crypto module contains all the functions and data types related to
cryptography.  This includes random number generation, encryption and
decryption, key generation, operations on nonces and generating random nonces.

\input{src/tox/Network/Tox/Crypto/Text.lhs}
\input{src/tox/Network/Tox/Crypto/Key.lhs}
\input{src/tox/Network/Tox/Crypto/KeyPair.lhs}
\input{src/tox/Network/Tox/Crypto/CombinedKey.lhs}
\input{src/tox/Network/Tox/Crypto/Nonce.lhs}
\input{src/tox/Network/Tox/Crypto/Box.lhs}

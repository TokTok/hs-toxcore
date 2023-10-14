\chapter{Crypto}

\begin{code}
{-# LANGUAGE StrictData #-}
module Network.Tox.Crypto where
\end{code}

The Crypto module contains all the functions and data types related to
cryptography.  This includes random number generation, encryption and
decryption, key generation, operations on nonces and generating random nonces.

\input{src/Network/Tox/Crypto/Key.lhs}
\input{src/Network/Tox/Crypto/KeyPair.lhs}
\input{src/Network/Tox/Crypto/CombinedKey.lhs}
\input{src/Network/Tox/Crypto/Nonce.lhs}
\input{src/Network/Tox/Crypto/Box.lhs}

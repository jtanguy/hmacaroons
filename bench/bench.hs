{-#LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import Criterion.Main

import Crypto.Macaroon
import Crypto.Macaroon.Internal


loc :: ByteString
loc = "http://mybank/"

ident :: ByteString
ident = "we used our secret key"

key :: ByteString
key = "this is our super secret key; only we should know it"

cav :: ByteString
cav = "test = caveat"


{-#INLINE benchCreate#-}
benchCreate :: (Key, Key, Location) -> Macaroon
benchCreate (secret, ident, loc) = create secret ident loc

{-#INLINE benchMint #-}
benchMint :: ((Key, Key, Location), ByteString) -> Macaroon
benchMint (ms,c) = addFirstPartyCaveat c (benchCreate ms)

main = defaultMain [
    bgroup "Crypto.Macaroon" [ bench "create"  $ nf benchCreate (key,ident,loc)
                             , bench "mint"  $ nf benchMint ((key,ident,loc),cav)
                             ]
    ]

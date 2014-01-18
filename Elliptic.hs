module Elliptic where

import System.Random (RandomGen , randomR)
import Data.Ix (inRange)
import Control.Applicative ((<$>) , empty)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe

--The modular multiplicative inverse via the extended Euclidean algorithm
inv :: Integer -> Integer -> Integer
inv = xEuclid 1 0 0 1 where
      xEuclid x0 y0 x1 y1 u v
       | v == 0    = x0
       | otherwise = let (q , r) = u `divMod` v
                     in xEuclid x1 y1 (x0-q*x1) (y0-q*y1) v r

--Data Structures and Operations on Elliptic Curves

 --Data

--A point on an elliptic curve either is a pair Point x y or is the point at Infinity
data Point = Point Integer Integer | Infinity deriving (Show,Eq)

--An elliptic curve is y^2 - x^3 - a*x^2 - b == 0 `mod` p with parameters: base point g of order n and cofactor h
data Curve = Curve {aParameter :: Integer,
                    bParameter :: Integer,
                    pParameter :: Integer,
                    gParameter :: Point,
                    nParameter :: Integer,
                    hParameter :: Integer}

--The Standards for Efficient Cryptography recommended elliptic curve domain parameters
secp192k1,secp192r1,secp224k1,secp224r1,secp256k1,secp256r1,secp384r1,secp521r1::Curve

secp192k1 = Curve
 {aParameter = 0x0,
  bParameter = 0x3,
  pParameter = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFEE37,
  gParameter = Point
               0xDB4FF10EC057E9AE26B07D0280B7F4341DA5D1B1EAE06C7D
               0x9B2F2F6D9C5628A7844163D015BE86344082AA88D95E2F9D,
  nParameter = 0xFFFFFFFFFFFFFFFFFFFFFFFE26F2FC170F69466A74DEFD8D,
  hParameter = 0x1}

secp192r1 = Curve
 {aParameter = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC,
  bParameter = 0x64210519E59C80E70FA7E9AB72243049FEB8DEECC146B9B1,
  pParameter = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF,
  gParameter = Point
               0x188DA80EB03090F67CBF20EB43A18800F4FF0AFD82FF1012
               0x07192B95FFC8DA78631011ED6B24CDD573F977A11E794811,
  nParameter = 0xFFFFFFFFFFFFFFFFFFFFFFFF99DEF836146BC9B1B4D22831,
  hParameter = 0x1}

secp224k1 = Curve
 {aParameter = 0x0,
  bParameter = 0x5,
  pParameter = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFE56D,
  gParameter = Point
               0xA1455B334DF099DF30FC28A169A467E9E47075A90F7E650EB6B7A45C
               0x7E089FED7FBA344282CAFBD6F7E319F7C0B0BD59E2CA4BDB556D61A5,
  nParameter = 0x10000000000000000000000000001DCE8D2EC6184CAF0A971769FB1F7,
  hParameter = 0x1}

secp224r1 = Curve
 {aParameter = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFE,
  bParameter = 0xB4050A850C04B3ABF54132565044B0B7D7BFD8BA270B39432355FFB4,
  pParameter = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000001,
  gParameter = Point
               0xB70E0CBD6BB4BF7F321390B94A03C1D356C21122343280D6115C1D21
               0xBD376388B5F723FB4C22DFE6CD4375A05A07476444D5819985007E34,
  nParameter = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFF16A2E0B8F03E13DD29455C5C2A3D,
  hParameter = 0x1}

secp256k1 = Curve
 {aParameter = 0x0,
  bParameter = 0x7,
  pParameter = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F,
  gParameter = Point
               0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
               0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8,
  nParameter = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141,
  hParameter = 0x1}

secp256r1 = Curve
 {aParameter = 0xFFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFC,
  bParameter = 0x5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B,
  pParameter = 0xFFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF,
  gParameter = Point
               0x6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296
               0x4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5,
  nParameter = 0xFFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551,
  hParameter = 0x1}

secp384r1 = Curve
 {aParameter = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFC,
  bParameter = 0xB3312FA7E23EE7E4988E056BE3F82D19181D9C6EFE8141120314088F5013875AC656398D8A2ED19D2A85C8EDD3EC2AEF,
  pParameter = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFF,
  gParameter = Point
               0xAA87CA22BE8B05378EB1C71EF320AD746E1D3B628BA79B9859F741E082542A385502F25DBF55296C3A545E3872760AB7
               0x3617DE4A96262C6F5D9E98BF9292DC29F8F41DBD289A147CE9DA3113B5F0B8C00A60B1CE1D7E819D7A431D7C90EA0E5F,
  nParameter = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC7634D81F4372DDF581A0DB248B0A77AECEC196ACCC52973,
  hParameter = 0x1}

secp521r1 = Curve
 {aParameter = 0x01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC,
  bParameter = 0x0051953EB9618E1C9A1F929A21A0B68540EEA2DA725B99B315F3B8B489918EF109E156193951EC7E937B1652C0BD3BB1BF073573DF883D2C34F1EF451FD46B503F00,
  pParameter = 0x01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF,
  gParameter = Point
               0x00C6858E06B70404E9CD9E3ECB662395B4429C648139053FB521F828AF606B4D3DBAA14B5E77EFE75928FE1DC127A2FFA8DE3348B3C1856A429BF97E7E31C2E5BD66
               0x011839296A789A3BC0045C8A5FB42C7D1BD998F54449579B446817AFBD17273E662C97EE72995EF42640C550B9013FAD0761353C7086A272C24088BE94769FD16650,
  nParameter = 0x01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA51868783BF2F966B7FCC0148F709A5D03BB5C9B8899C47AEBB6FB71E91386409,
  hParameter = 0x1}
                    
 --Operations use the Reader monad to express their dependence on choice of Curve.

--Addition of two Points pt1 .+ pt2
(.+) :: Point -> Point -> Reader Curve Point
Infinity .+ pt = return pt
pt .+ Infinity = return pt
(Point x1 y1) .+ (Point x2 y2)
 = do p <- reader pParameter
      if (x1-x2) `mod` p == 0 && (y1+y2) `mod` p == 0 -- case of additive inverses pt1 .+ pt2 == Infinity
      then return Infinity
      else do a <- reader aParameter
              let m = if (x1-x2) `mod` p == 0 && (y1-y2) `mod` p == 0 -- case of pt1 == pt2
                      then (3*x1^2+a) * inv (2*y1) p -- slope of tangent at pt1 == pt2
                      else (y2-y1)  *  inv (x2-x1) p -- slope of secant for pt1 /= pt2
                  x3 = (m^2-x1-x2)    `mod` p
                  y3 = (m*(x1-x3)-y1) `mod` p
              return (Point x3 y3)

double :: Point -> Reader Curve Point
double pt = pt .+ pt

--Scalar multiplication of a Point k .* pt
(.*) :: Integer -> Point -> Reader Curve Point
k .* pt | k == 0 || pt == Infinity = return Infinity
        | odd  k                   = (k-1) .* pt       >>= (.+ pt)
        | even k                   = (k `div` 2) .* pt >>= double

--Linear combination (k1 .* pt1) .+ (k2 .* pt2) can be accomplished naively using addition and scalar multiplication but this is a speedier algorithm
comb :: (Integer , Point) -> (Integer , Point) -> Reader Curve Point
(k1 , pt1) `comb` (k2 , pt2)
 | k1 == 0 || pt1 == Infinity = k2 .* pt2
 | k2 == 0 || pt2 == Infinity = k1 .* pt1
 | odd  k1 && even k2         = (k1-1 , pt1) `comb` (k2   , pt2) >>= (.+ pt1)
 | even k1 && odd  k2         = (k1   , pt1) `comb` (k2-1 , pt2) >>= (.+ pt2)
 | odd  k1 && odd  k2         = (k1-1 , pt1) `comb` (k2-1 , pt2) >>= (.+ pt1) >>= (.+ pt2)
 | even k1 && even k2         = (k1 `div` 2 , pt1) `comb` (k2 `div` 2 , pt2)  >>= double

--Digital signature data structures and operations
type PrivateKey = Integer
type PublicKey  = Point
type KeyPair    = (PrivateKey , PublicKey)
type Message    = Integer
type Signature  = (Integer , Integer)

--unprivate is the "one-way" function; it's also a homomorphism of Abelian groups!
unprivate :: PrivateKey -> Reader Curve PublicKey
unprivate private = (private .*) =<< reader gParameter

 --Operations use the monad transformer RandomGen rg => StateT rg to express their dependence on pseudorandomness

--Randomly generate a new PrivateKey
newPrivateKey :: RandomGen rg => StateT rg (Reader Curve) PrivateKey
newPrivateKey = do n <- lift (reader nParameter)
                   state (randomR (1 , n-1))

--Randomly generate a new KeyPair
newKeyPair :: RandomGen rg => StateT rg (Reader Curve) KeyPair
newKeyPair = do private <- newPrivateKey
                public  <- lift (unprivate private)
                return (private , public)

--Digitally sign a Message with a PrivateKey
sign :: RandomGen rg => PrivateKey -> Message -> StateT rg (Reader Curve) Signature
sign d e = do n <- lift (reader nParameter)
              (k , Point x _) <- newKeyPair
              let r = x `mod` n
                  s = (inv k n) * (e + d*r) `mod` n
              if r == 0 || s == 0
              then sign d e
              else return (r , s)

 --Operations using the monad transfomer MaybeT are composable validity checkers

check :: (Functor m , Monad m) => Bool -> MaybeT m ()
check True  = return ()
check False = empty

checkPublicKey :: PublicKey -> MaybeT (Reader Curve) ()
checkPublicKey Infinity  = return ()
checkPublicKey (Point x y) = do p <- lift (reader pParameter)
                                a <- lift (reader aParameter)
                                b <- lift (reader bParameter)
                                check $ y^2 - x^3 - a*x^2 - b == 0 `mod` p

checkKeyPair :: KeyPair -> MaybeT (Reader Curve) ()
checkKeyPair (private , public) = do checkPublicKey public
                                     pub <- lift (unprivate private)
                                     check $ pub == public

--Check validity of a Signature on a Message with a PublicKey
checkSig :: PublicKey -> Message -> Signature -> MaybeT (Reader Curve) ()
checkSig q e (r , s) = do checkPublicKey q
                          n  <- lift (reader nParameter)
                          check $ all (inRange (1 , n-1)) [r , s]
                          let w  = inv s n
                              u1 = e * w `mod` n
                              u2 = r * w `mod` n
                          g  <- lift (reader gParameter)
                          pt <- lift ((u1 , g) `comb` (u2 , q))
                          check $ pt /= Infinity
                          let Point x _ = pt
                              v         = x `mod` n
                          check $ v == r

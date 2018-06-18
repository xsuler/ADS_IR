{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Main where
import           Debug.Trace
import           Control.Monad
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as L
import           Data.List
import           Data.Serialize               (decodeLazy, encodeLazy)
import qualified Data.Vector.Storable         as V
import           Grenade
import           Numeric.LinearAlgebra        (maxIndex)
import qualified Numeric.LinearAlgebra.Static as S
import           System.Random
import           Graphics.Image              as I hiding (map)
import           Graphics.Image.Interface    as I hiding (map)
import           Graphics.Image.Processing   as I hiding (map)
import qualified Numeric.LinearAlgebra as NLA
import qualified Numeric.LinearAlgebra.Static as NLA
import qualified Numeric.LinearAlgebra.Data as NLA

readF::FilePath->IO (S ('D3 100 100 3))
readF path =do
     Right x<- (fmap (resize Bilinear Edge (100,100)))<$>readImage path::IO (Either String (Image VS RGB Double))
     let Just res=fromStorable $V.concatMap (\(PixelRGB r g b) -> V.fromList [r,g,b]) $ toVector x
     return res

scons a !b = V.cons a b

elemAt = (V.!)

toImg::S ('D3 100 100 3)->Image VS RGB Double
toImg x = fromVector (100,100) $groupRGB $ext x
          where groupRGB v= if V.null v then V.empty else (PixelRGB (v `elemAt` 0) (v `elemAt` 1) (v `elemAt` 2)) `scons` groupRGB (V.drop 3 v)
                ext::S ('D3 100 100 3)->V.Vector Double
                ext (S3D m) = NLA.flatten$NLA.extract m

 type FE
  = Network
    '[ Convolution 3 24 5 5 1 1,  Relu, Pooling 2 2 2 2
     , Reshape
     , FullyConnected 55296 1024, Logit
     , FullyConnected 1024 55296, Relu
     , Reshape
     , Deconvolution 24 3 6 6 2 2, Logit
     ]
    '[ 'D3 100 100 3, 'D3 96 96 24, 'D3 96 96 24, 'D3 48 48 24 
     , 'D1 55296
     , 'D1 1024, 'D1 1024
     , 'D1 55296, 'D1 55296
     , 'D3 48 48 24
     , 'D3 100 100 3,'D3 100 100 3]


trainOne::LearningParameters->FE->S ('D3 100 100 3)->FE
trainOne rate !net i=
  train rate net i i

trainT::FE->Int->S ('D3 100 100 3)->FE
trainT !net x img= 
    let lp = LearningParameters 0.0001 0.9 0.0005
    in  foldl' (\n i -> 
      trace (show i) $trainOne (lp{ learningRate = learningRate lp *(0.999 ^ i)}) n img)
      net [0..x]

main' = do
  i<-readF "image/n11669921_50878.JPEG"
  writeImage "newimg.jpg" $ toImg i

main = do
  i<-readF "image/n11669921_50878.JPEG"
  x<-Prelude.read<$>getLine
  net<-randomNetwork::IO FE
  let net'=trainT net x i
  writeImage "newimg.jpg" $ toImg $runNet net' i

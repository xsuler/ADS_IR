{-# language DataKinds #-}
import Linear.V2
import Data.Word
import Prelude as P
import Debug.Trace
import OpenCV.Features2d
import OpenCV
import Data.Default.Class
import Data.Vector as V hiding ((++))
import qualified Data.ByteString as B
import System.IO
import Data.List.Split

vsort::(Num a)=>V.Vector a->V.Vector a
vsort v
 |V.null v = V.empty
 |otherwise = V.concat [vsort smaller,V.singleton hd,vsort larger]
             where hd=V.head v
                   tl=V.tail v
                   smaller=V.filter (<hd) tl
                   larger=V.filter (>=hd) tl

vsort::(Num a)=>V.Vector a->V.Vector a
pvsort v
 |V.null v = V.empty
 |otherwise = V.concat [pvsort smaller,V.singleton hd,pvsort larger]
             where hd=V.head v
                   tl=V.tail v
                   smaller=V.filter ((<snd hd).snd) tl
                   larger=V.filter ((>=snd hd).snd) tl

fbMatcherImg imgq imgt= do
    let (_, descs1) = exceptError $ orbDetectAndCompute orb imgq Nothing
    let (_, descs2) = exceptError $ orbDetectAndCompute orb imgt Nothing
    fbmatcher <- newFlannBasedMatcher (def { indexParams = FlannLshIndexParams 20 10 2 })
    ls<-(vsort.V.map (dmatchDistance.dmatchAsRec)) <$> match fbmatcher descs1 descs2 Nothing
    return $V.sum $V.take 20 ls
  where
    orb = mkOrb defaultOrbParams {orb_nfeatures = 50} 

ilist path = lines<$>readFile path

btPrint q res = q++":"++(P.tail$P.concat$V.toList$V.map ((',':).P.drop 6.fst) res)

main = do
  xs<-ilist "result0"
  h<-openFile "result" WriteMode
  flip P.mapM_ (P.zip [1..] xs) $ \(i,q)-> do
    print i
    let nq = P.takeWhile (/=':') q
    let pathq = (P.++) "qmage/" $P.takeWhile (/=':') q
    let ls= P.map ((P.++) "image/") $ splitOn "," $ P.tail $P.dropWhile (/=':') q
    img' <-  imdecode ImreadUnchanged <$> B.readFile pathq
    let img= exceptError $ resize (ResizeAbs $ toSize $V2 160 160) InterArea img'
    ts<-P.mapM (\path->do 
         img1'<- imdecode ImreadUnchanged <$> B.readFile path
         let img1= exceptError$ resize (ResizeAbs $ toSize $V2 160 160) InterArea img1'
         res<-fbMatcherImg img img1
         return (path,res)
         ) ls
    hPutStrLn h $btPrint nq $ V.take 10 $pvsort$V.fromList ts
  hClose h


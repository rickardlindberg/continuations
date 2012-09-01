-- The purpose of this file is to get a feel for how continuation-passing style
-- works.

pythagorasCont :: Float -> Float -> (Float -> IO ()) -> IO ()

pythagorasCont x y k =
    timesCont x   x  $ \x2 ->
    timesCont y   y  $ \y2 ->
    plusCont  x2  y2 $ \sum ->
    sqrtCont  sum k

plusCont x y k = k $ x + y

timesCont x y k = k $ x * y

sqrtCont x k = k $ sqrt x

main = do
    let k res = print res
    pythagorasCont 2 5 k

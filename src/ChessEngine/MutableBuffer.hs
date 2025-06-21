{-# LANGUAGE BangPatterns #-}
module ChessEngine.MutableBuffer
    ( MutableBuffer
    , create
    , setBufferSize
    , reset
    , push
    , pushMany
    , fold 
    , range
    , partition
    , partition'
    , sort
    , bufferSize
    )

where
import Data.Vector.Unboxed.Mutable as UM
import Data.Primitive.PrimVar
import Control.Monad.IO.Class
import Control.Monad as M
import Data.List.Fusion.Probe


bufferMaxSize = 200

data MutableBuffer a = MutableBuffer {
    size :: !(PrimVar (PrimState IO) Int),
    content :: !(IOVector a)
}

create :: Unbox a => IO (MutableBuffer a)
create = do 
    content <- unsafeNew bufferMaxSize
    size <- newPrimVar 0
    return $ MutableBuffer { size = size, content = content }

reset :: MutableBuffer a -> IO ()
reset b = setBufferSize b 0

setBufferSize :: MutableBuffer a -> Int -> IO ()
setBufferSize MutableBuffer { size = size } s =
    writePrimVar size s

{-# INLINE push #-}
push :: Unbox a => MutableBuffer a -> a -> IO ()
push MutableBuffer { size = size, content = content } el =
    do
        s <- readPrimVar size
        writePrimVar size (s + 1)
        unsafeWrite content s el

{-# INLINE pushMany #-}
pushMany :: Unbox a => MutableBuffer a -> [a] -> IO ()
pushMany b els =
    M.forM_ (fuseThis els) $ push b

{-# INLINE fold #-}
fold :: Unbox e => MonadIO m => (a -> e -> m a) -> a -> MutableBuffer e -> m a
fold updater init MutableBuffer { content = content, size = size } =
    let updater' state index =
          do
            el <- liftIO $ UM.unsafeRead content index
            updater state el
    in do 
        s <- liftIO $ readPrimVar size
        M.foldM updater' init [0 .. s - 1]

bufferSize :: MutableBuffer e -> IO Int
bufferSize MutableBuffer { size = size } =
    readPrimVar size

range :: MutableBuffer e -> IO (Int, Int)
range MutableBuffer { size = size } =
    do
        s <- readPrimVar size
        return (0, s)

{-# INLINE partition #-}
partition :: Unbox e => (e -> Bool) -> MutableBuffer e -> IO ((Int, Int), (Int, Int))
partition pred buffer =
    do 
        s <- readPrimVar $ size buffer
        partition' pred buffer (0, s)

{-# INLINE partition' #-}
partition' :: Unbox e => (e -> Bool) -> MutableBuffer e -> (Int, Int) -> IO ((Int, Int), (Int, Int))
partition' pred MutableBuffer { content = content } (start, end) =
    do 
        matchedCount <- M.foldM update 0 [start .. end - 1]
        return ((start, start + matchedCount), (start + matchedCount, end))
  where
    update matchedCount index = do
        el <- UM.unsafeRead content index
        let !match = pred el
        let !needSwap = match && index > matchedCount
        let !newMatchedCount = if match then matchedCount + 1 else matchedCount
        when needSwap $ do
            let swapIndex = matchedCount + start
            unsafeSwap content index swapIndex
        return newMatchedCount

sort :: Unbox e => (e -> e -> Ordering) -> MutableBuffer e -> (Int, Int) -> IO ()
sort cmp MutableBuffer { content = content } (start, end) =
    M.forM_ 
        [0 .. end - 1 - start] 
        (\i -> M.forM_ 
                    [start .. end - 1 - i - 1]
                    (\j -> testAndSwap j (j + 1)))
  where
    testAndSwap i1 i2 =
        do 
            el1 <- UM.unsafeRead content i1
            el2 <- UM.unsafeRead content i2
            let shouldSwap = cmp el1 el2 == GT
            when shouldSwap $
                unsafeSwap content i1 i2

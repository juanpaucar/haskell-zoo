{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Vector.Sample where

import Data.Word                (Word8)
import Data.Proxy               
import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr)
import Foreign.Ptr              (Ptr, plusPtr)
import Foreign.Storable         (peekByteOff, pokeByteOff, sizeOf, Storable(..))
import Foreign.C.Types          (CSize(..))

import GHC.ForeignPtr           (ForeignPtr(ForeignPtr), mallocPlainForeignPtrBytes)
import GHC.Ptr                  (Ptr(..), castPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)

data IVector a = Nil
               | Cons a (IVector a)

instance Functor IVector where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

data MVector a = MV {-# UNPACK #-} !(ForeignPtr a) -- data
                    {-# UNPACK #-} !Int            -- offset
                    {-# UNPACK #-} !Int            -- length

sizeOfVector :: forall a. Storable a => a -> Int -> Int
sizeOfVector _ l = fromIntegral (l * sizeOf (undefined :: a))

create :: forall a. Storable a => Int -> (Ptr a -> IO ()) -> IO (MVector a)
create l f = do
    fp <- mallocPlainForeignPtrBytes (sizeOfVector (undefined :: a) l)
    withForeignPtr fp $ \p -> f p
    return $! MV fp 0 l

unsafeCreate :: Storable a => Int -> (Ptr a -> IO ()) -> MVector a
unsafeCreate l f = unsafeDupablePerformIO (create l f)

map' :: (Storable a, Storable b) => (a -> b) -> MVector a -> MVector b 
map' f (MV fp s len) = unsafeDupablePerformIO $ withForeignPtr fp $ \a ->
  create len $ map_ 0 (a `plusPtr` s)
    where
      map_ :: Int -> Ptr a -> Ptr a -> IO ()
      map_ !n !p1 !p2
        | n >= len = return ()
        | otherwise = do
          x <- peekByteOff p1 n
          pokeByteOff p2 n (f x)
          map_ (n+1) p1 p2

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

memcpy :: forall a. Storable a => Ptr a -> Ptr a -> Int -> IO ()
memcpy p q s = c_memcpy (castPtr p) (castPtr q) (fromIntegral $ sizeOfVector (undefined :: a) s) >> return ()

concat :: forall a. Storable a => MVector a -> MVector a -> MVector a
concat (MV _ _ 0) b = b
concat a (MV _ _ 0) = a
concat (MV fpa  offa lena) (MV fpb offb lenb) =
  unsafeCreate (lena + lenb) $ \ptra -> do
    let ptrb = ((ptra `plusPtr` sizeOfVector (undefined :: a) lena) :: Ptr a)
    withForeignPtr fpa $ \pa -> memcpy ptra (pa `plusPtr` offa) lena
    withForeignPtr fpb $ \pb -> memcpy ptrb (pb `plusPtr` offb) lenb

  

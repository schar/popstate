module Data.Reader where

newtype Reader r a =
  Reader
    { runReader :: r -> [a]
    }

asks :: (r -> a) -> Reader r a
asks f = Reader $ \r -> [f r]

ret :: a -> Reader r a
ret x = Reader $ const [x]

bind :: Reader r a -> (a -> Reader r b) -> Reader r b
m `bind` k = Reader $ \r -> concat [runReader (k x) r | x <- runReader m r]

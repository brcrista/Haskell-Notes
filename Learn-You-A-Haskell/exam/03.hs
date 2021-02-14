import Prelude(Functor, Applicative, Monad)

fmap  :: Functor f     =>   (a -> b) -> f a -> f b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
(>>=) :: Monad m       => m a -> (a -> m b) -> m b
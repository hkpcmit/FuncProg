data Maybe1 a = Nothing1
             | Just1 a
             deriving Show

instance Monad Maybe1 where
        return x = Just1 x
        Nothing1 >>= _ = Nothing1
        (Just1 x) >>= f = f x

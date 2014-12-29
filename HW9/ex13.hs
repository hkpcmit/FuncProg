class Monoid a where
        mempty :: a
        (<>) :: a -> a -> a

class (Functor f) => Foldable f where
        fold :: (Monoid m) => f m -> m

instance Foldable [] where
        fold = foldr (<>) mempty


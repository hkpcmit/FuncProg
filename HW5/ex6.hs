mrep :: Int -> a -> [a]
mrep 0 _ = []
mrep n x = x : mrep (n-1) x

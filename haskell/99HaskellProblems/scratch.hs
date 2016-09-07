import System.Random (randomRIO)
 
randomPermu :: [a] -> IO [a]
randomPermu [] = return []
randomPermu (x:xs) = do
  rand <- randomRIO (0, (length xs))
  rest <- randomPermu xs
  return $ let (ys,zs) = splitAt rand rest
           in ys++(x:zs)
 
randomPermu' [] = return []
randomPermu' xs = do
    rand <- randomRIO (0, (length xs)-1)
    rest <- let (ys,(_:zs)) = splitAt rand xs
            in randomPermu' $ ys ++ zs
    return $ (xs!!rand):rest

-------------------------------------------------------------------------------
-- generation of mapCCC and friends
-------------------------------------------------------------------------------

funs =
    [ ('I', "I", "")
    , ('K', "K _", "_") 
    , ('C', "C", "TExpQ")
    ]

names2 = flip mapM [ (xx,yy) | xx@(x,xr,xl)<- funs, yy@(y,yr,yl) <- funs, x == 'C' || y == 'C' ] $
      \((x,xr,xl),(y,yr,yl)) -> do
          putStrLn $ "map" ++ [x,y] ++ ","

names3 = do
   flip mapM_ [ (xx,yy,zz) | xx@(x,xr,xl)<- funs, yy@(y,yr,yl) <- funs, zz@(z,zr,zl) <- funs, x == 'C' || y == 'C' || z == 'C' ] $
      \((x,xr,xl),(y,yr,yl),(z,zr,zl)) -> do
          putStrLn $ "map" ++ [x,y,z] ++ ","

gen2 = do
   flip mapM_ [ (xx,yy) | xx@(x,xr,xl)<- funs, yy@(y,yr,yl) <- funs, x == 'C' || y == 'C' ] $
      \((x,xr,xl),(y,yr,yl)) -> do
          putStrLn $ "map" ++ [x,y]
              ++ " :: ("++ xl ++ " x -> " ++ yl ++ " x) -> "
              ++ xr ++ " x -> " ++ yr ++ " x"

          putStrLn $ "map" ++ [x,y]
              ++ " f (" ++ xr ++ " a) = " ++ yr ++ "(f a)"
      
gen3 = do
   flip mapM_ [ (xx,yy,zz) | xx@(x,xr,xl)<- funs, yy@(y,yr,yl) <- funs, zz@(z,zr,zl) <- funs, x == 'C' || y == 'C' || z == 'C' ] $
      \((x,xr,xl),(y,yr,yl),(z,zr,zl)) -> do
          putStrLn $ "map" ++ [x,y,z]
              ++ " :: ("++ xl ++ " x -> " ++ yl ++ " x -> " ++ zl ++ " x) -> "
              ++ xr ++ " x -> " ++ yr ++ " x -> " ++ zr ++ " x"

          putStrLn $ "map" ++ [x,y,z]
              ++ " f (" ++ xr ++ " a) (" ++ yr ++ " b) = " ++ zr ++ " (f a b)"
    

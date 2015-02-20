calc n = c n [n] 
	where 
		c n l | n == 1 = reverse $ l
		      | odd n = c (f_odd n) ((f_odd n):l)  
       		      | otherwise = c (f_even n) (f_even n:l)
		f_odd x = 3*x + 1
		f_even x = x `div` 2
			
calcs n = c n [(0,0)] 
	where 
		c n l | n == 1 = reverse l
		      | odd n = c (f_odd n) $ (0,1):[(a+1,b+1) | (a,b) <- l]  
       		      | otherwise = c (f_even n) [(a,b+1) | (a,b) <- l]
		f_odd x = (3*x + 1) `div` 2
		f_even x = x `div` 2
	
to_one n ((a,b):xs) = n*(3**(fromInteger a)/2**(fromInteger b)) + (sum $ [3**(fromInteger i)/2**(fromInteger j) | (i,j) <- xs])

find_x eks@(a:e_k) = 2**(fromIntegral a)/3**(fromIntegral $ k - 1) * (1 - sum_ek)
	where 
		sum_ek = sum $ [ 3**(fromIntegral i)/2**(fromIntegral e) | (i, e) <- zip [k-2,k-3..0] e_k ]
	      	k = length eks

to_latex n = (show n) ++ " = \\frac{2^{" ++ e_k ++ "}}{3^{" ++  show k ++  "}}\\Big[ 1 - " ++ the_sum ++ "\\Big]"
	where 
		ns = calcs n	
		(e_k:e) = map (show.snd) ns 
		k = length e	
		the_sum_l = map (\(a,b) -> "\\frac{3^{" ++ show a ++ "}}{2^{" ++ show b ++ "}}") $ tail ns
		the_sum = foldl (\a b -> a ++ " - " ++ b) (head the_sum_l) $ tail the_sum_l

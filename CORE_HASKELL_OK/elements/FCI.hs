zyVBZZFRDbFi1ZZV_SystemziConsoleziParseArgs_getRequiredArg_info ghczmprim_GHCziTypes_Module_con_info parseargszm0zi2zi0zi9zm566ltZZ8uzzyVBZZFRDbFi1ZZV_SystemziConsoleziParseArgs_getArgStdio2_info parseargszm0zi2zi0zi9zm566ltZZ8uzzyVBZZFRDbFi1ZZV_SystemziConsoleziParseArgs_zdtczqArgsHardDash_closure parseargszm0zi2zi0zi9zm566ltZZ8uzzyVBZZFRDbFi1ZZV_SystemziConsoleziParseArgs_getArgStdio_closure parseargszm0zi2zi0zi9zm566ltZZ8uzzyVBZZFRDbFi1ZZV_SystemziConsoleziParseArgs_zdwzdcshow_closure parseargszm0zi2zi0zi9zm566ltZZ8uzzyVBZZFRDbFi1ZZV_SystemziConsoleziParseArgs_zdtrModule3_closure parseargszm0zi2zi0zi9zm566ltZZ8uzzyVBZZFRDbFi1ZZV_SystemziConsoleziParseArgs_Arg_closure parseargszm0zi2zi0zi9zm566ltZZ8uzzyVBZZFRDbFi1ZZV_SystemziConsoleziParseArgs_dataArgOptional_info base_GHCziRead_zdfReadIntegerzuzdsreadNumber_info ghczmprim_GHCziCString_unpackAppendCStringzh_info                                                                                                                                                    = dm
	  dp = -ap / (1 - ap)
 	  dm =  am / (1 - am)
 	  ap = magnitude (x!(k+1)) / magnitude (x!k)
 	  am = magnitude (x!(k-1)) / magnitude (x!k)
 	  n = snd (bounds x) + 1

-- | Quinn's Second Estimator (FCI2)

quinn2 :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ X[k]
       -> a -- ^ k
       -> b -- ^ w

quinn2 x k = 2 * pi * ((fromIntegral k) + d) / (fromIntegral n)
    where d = (dp + dm) / 2 + tau(dp^!2) - tau(dm^!2)
          dp = -ap / (1 - ap)
 	  dm =  am / (1 - am)
 	  ap = magnitude (x!(k+1)) / magnitude (x!k)
 	  am = magnitude (x!(k-1)) / magnitude (x!k)
 	  tau y = 0.25 * log10(3*y^!2 + 6 * y + 1) - (sqrt 6) / 24 * log10 ((y + 1 - sqrt (2/3)) / (y + 1 + sqrt (2/3)))
 	  n = snd (bounds x) + 1

-- | Quinn's Third Estimator (FCI3)

quinn3 :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ X[k]
       -> a -- ^ k
       -> b -- ^ w

quinn3 x k = 2 * pi * ((fromIntegral k) + d) / (fromIntegral n)
    where d = (dm + dp) / 2 + (dp - dm) * (3*dt^!3 + 2*dt) / (3*dt^!4+6*dt^!2+1)
	  dt | dm > 0 && dp > 0 = dp
	     | otherwise        = dm
	  dp = -ap / (1 - ap)
 	  dm =  am / (1 - am)
 	  ap = magnitude (x!(k+1)) / magnitude (x!k)
 	  am = magnitude (x!(k-1)) / magnitude (x!k)
 	  n = snd (bounds x) + 1

-- | Eric Jacobsen's Estimator

jacobsen :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ X[k]
       -> a -- ^ k
       -> b -- ^ w

jacobsen x k = 2 * pi * ((fromIntegral k) + d) / (fromIntegral n)
    where d = realPart ((x!(k-1) - x!(k+1)) / (2 * x!k - x!(k-1) - x!(k+1)))
 	  n = snd (bounds x) + 1

-- | MacLeod's Three Point Estimator

macleod3 :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ X[k]
       -> a -- ^ k
       -> b -- ^ w

macleod3 x k = 2 * pi * ((fromIntegral k) + d) / (fromIntegral n)
    where rm1 = realPart (x!(k-1) * conjugate (x!k))
 	  r   = realPart (x!k     * conjugate (x!k))
 	  rp1 = realPart (x!(k+1) * conjugate (x!k))
	  d = (sqrt (1 + 8 * g^!2) - 1) / 4 / g
 	  g = (rm1 - rp1) / (2 * r + rm1 + rp1)
 	  n = snd (bounds x) + 1

-- | MacLeod's Three Point Estimator

macleod5 :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ X[k]
       -> a -- ^ k
       -> b -- ^ w

macleod5 x k = 2 * pi * ((fromIntegral k) + d) / (fromIntegral n)
     where rm2 = realPart (x!(k-2) * conjugate (x!k))
	   rm1 = realPart (x!(k-1) * conjugate (x!k))
 	   r   = realPart (x!k     * conjugate (x!k))
 	   rp1 = realPart (x!(k+1) * conjugate (x!k))
 	   rp2 = realPart (x!(k+2) * conjugate (x!k))
	   d = 0.4041 * atan (2.93 * g)
 	   g = (4 * (rm1 - rp1) + 2 * (rm2 - rp2)) / (12 * r + 8 * (rm1 + rp1) + rm2 + rp2)
 	   n = snd (bounds x) + 1

-- | Rife and Vincent's Estimator

rv :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ X[k]
       -> a -- ^ k
       -> b -- ^ w

rv x k = 2 * pi * ((fromIntegral k) + d) / (fromIntegral n)
    where d = fromIntegral at * magnitude (x!(k+at) / x!k) / (1 + magnitude (x!(k+at) / x!k))
	  at | (magnitude (x!(k+1)))^!2 > (magnitude (x!(k-1)))^!2 =  1
	     | otherwise                                         = -1
 	  n = snd (bounds x) + 1

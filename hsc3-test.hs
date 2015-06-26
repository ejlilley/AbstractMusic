import Sound.SC3



-- withSC3 (send (g_new [(1, AddToTail, 0)]))
-- 
-- audition (out 0 (sinOsc AR 440 0 * 0.1))
-- 
-- withSC3 (send (n_free [1]))
-- 
-- withSC3 reset
-- 
-- audition (out 0 (sinOsc AR 220 0 * 0.1))

foo = audition (out 0 (ppd s)) where
  ppd i = let a = localIn 2 AR + mce [i,0]
              b = delayN a 0.2 0.2
              c = mceEdit reverse b * 0.8
          in mrg [b,localOut c]
  n = whiteNoise 'α' AR
  s = decay (impulse AR 0.3 0) 0.1 * n * 0.2

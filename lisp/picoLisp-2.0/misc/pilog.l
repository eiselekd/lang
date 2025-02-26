# 19apr02abu
# (c) Software Lab. Alexander Burger

(be sister (@X @Y) (parents @X @M @F) (parents @Y @M @F) (different @X @Y))

(be parents (@C @M @F) (mother @C @M) (father @C @F))

(be mother (Mia Masako))
(be mother (Laila Masako))
(be mother (Mona Masako))

(be father (Mia Alex))
(be father (Laila Alex))
(be father (Mona Alex))

(be factorial (0 1) T)
(be factorial (@N @X)
   (@A - (-> @N) 1)
   (factorial @A @B)
   (@X * (-> @N) (-> @B)) )

(be fibo (0 1) T)
(be fibo (1 1) T)
(be fibo (@N @X)
   (@Y - (-> @N) 1)
   (@Z - (-> @N) 2)
   (fibo @Y @A)
   (fibo @Z @B)
   (@X + (-> @A) (-> @B))
   (asserta (fibo (@N @X) T)) )


(be int (@N)
   (@ zero *N)
   (repeat)
   (@N inc '*N) )

(be prnum ()
   (@ zero *N)
   (repeat)
   (@ println (inc '*N))
   (@ >= *N 4) )

(be gennum (@N)
   (@C box 0)
   (_gennum @N @C) )

(be _gennum (@N @C) (@ >= (val (-> @C)) 4) T (fail))
(be _gennum (@N @C) (@N inc (-> @C)))
(repeat)

(be genlst (@X)
   (@C box (1 2 3 4))
   (_genlst @X @C) )

(be _genlst (@X @C) (@ not (val (-> @C))) T (fail))
(be _genlst (@X @C) (@X pop (-> @C)))
(repeat)


### Tests ###
(test NIL (solve '((equal A B))))
(test '(T) (solve '((equal A A))))

(test NIL (solve '((not (equal A A)))))
(test '(T) (solve '((not (equal A B)))))

(test NIL (solve '((different A A))))
(test '(T) (solve '((different A B))))

(test
   '(((@X . 3)) ((@X . 4)))
   (solve '((or ((equal 3 @X)) ((equal 4 @X))))) )

(test '(T) (solve '((append (a b) (c d) (a b c d)))))
(test
   '(((@X) (@Y a b c)) ((@X a) (@Y b c)) ((@X a b) (@Y c)) ((@X a b c) (@Y)))
   (solve '((append @X @Y (a b c)))) )

(test '(T) (solve '((member b (a b c)))))
(test
   '(((@X . a)) ((@X . b)) ((@X . c)))
   (solve '((member @X (a b c)))) )

(test '(T) (solve '((clause append ((NIL @X @X))))))


(test
   '(((@B . Mia)) ((@B . Mona)))
   (solve '((sister Laila @B))) )

(test
   '(((@X . 1)) ((@X . 2)) ((@X . 3)) ((@X . 4)))
   (solve '((gennum @X))) )

(test
   '(((@X . 1)) ((@X . 2)) ((@X . 3)) ((@X . 4)))
   (solve '((genlst @X))) )

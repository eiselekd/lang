# 06nov01abu
# (c) Software Lab. Alexander Burger

(load "lib/simul.l")

# Set up distance properties
# See "misc/travel.l" and "doc/travel"
(mapc
   '((L)
      (put (car L) (cadr L) (caddr L))
      (put (cadr L) (car L) (caddr L)) )
   (quote
      (Rheine Muenster 39)
      (Rheine Osnabrueck 42)
      (Muenster Osnabrueck 51)
      (Warendorf Muenster 28)
      (Warendorf Osnabrueck 43)
      (Warendorf Rheda 24)
      (Warendorf Guetersloh 27)
      (Osnabrueck Bielefeld 48)
      (Rheda Guetersloh 10)
      (Bielefeld Guetersloh 16)
      (Bielefeld Paderborn 39)
      (Paderborn Guetersloh 31)
      (Paderborn Rheda 32)
      (Paderborn Soest 41)
      (Soest Rheda 38)
      (Soest Beckum 26)
      (Beckum Rheda 24)
      (Beckum Warendorf 27)
      (Ahlen Warendorf 27)
      (Ahlen Muenster 46)
      (Ahlen Beckum 11)
      (Ahlen Soest 27) ) )

# Find a route from 'A' to 'B'
(de route (A B Lst)
   (if (get A B)
      (list A B)
      (and
         (pick
            '((X)
               (and
                  (not (memq X Lst))
                  (route X B (cons A Lst)) ) )
            (shuffle (mapcar cdr (getl A))) )
         (cons A @) ) ) )

# Minimize trip from 'A' to 'B'
(de trip (Cnt A B)
   (gen Cnt
      '((X Y)  # Recombination
         (make
            (while (prog (link (pop 'X)) X)
               (when (member (car X) (cdr Y))
                  (setq Y @)
                  (xchg 'X 'Y) ) ) ) )
      '((L)  # Mutation
         (let (N (length L)  H (>> 1 N)  N1 (rand 1 H)  N2 (rand (+ 1 H) N))
            (if (route (get L N1) (get L N2))
               (append
                  (head (- N1 1) L) 
                  @
                  (nth L (+ N2 1)) )
               L ) ) )
      '((L)  # Selection
         (let A (pop 'L)
            (-
               (sum
                  '((X) (get A (setq A X)))
                  L ) ) ) )
      (route A B) ) )

# Optimum hit percentage
(de tst (Cnt)
   (let Ok 0
      (do 100
         (when
            (=
               (trip Cnt 'Rheine 'Paderborn)
               '(Rheine Muenster Warendorf Rheda Paderborn) )
            (inc 'Ok) ) )
      Ok ) )

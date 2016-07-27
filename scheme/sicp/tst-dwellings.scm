;;;; tests for dwellings.scm
(load "test-framework.scm")
(load "dwellings.scm")
(load "lib.scm")

(set-test-section! "sequences")

(test-eq "permutate-perms"
  (permutate-perms 'a '((b c) (d e))) 
  '((b c a) (b a c) (a b c) (d e a) (d a e) (a d e)))

(set-test-section! "predicates")

(test-eq "baker #t" (baker-not-5 '(m f c b s)) #t)
(test-eq "baker #f" (baker-not-5 '(m f c s b)) #f)

(test-eq "cooper #t" (cooper-not-1 '(m f c b s)) #t)
(test-eq "cooper #f" (cooper-not-1 '(c f m s b)) #f)

(test-eq "fletcher #t" (fletcher-not-1-or-5 '(m f c b s)) #t)
(test-eq "fletcher #f back" (fletcher-not-1-or-5 '(c b m s f)) #f)
(test-eq "fletcher #f front" (fletcher-not-1-or-5 '(f c m s b)) #f)

(test-eq "position" (position 'b '(m f b c s)) 3)

(test-eq "miller > cooper" (miller-gt-cooper '(m f b c s)) #f)
(test-eq "miller > cooper" (miller-gt-cooper '(b f m c s)) #f)
(test-eq "miller > cooper" (miller-gt-cooper '(b f c m s)) #t)
(test-eq "miller > cooper" (miller-gt-cooper '(c f b s m)) #t)

(test-eq "smith-fletcher-not-adjacent"
  (smith-fletcher-not-adjacent '(m f b c s)) #t)
(test-eq "smith-fletcher-not-adjacent"
  (smith-fletcher-not-adjacent '(f m s c b)) #t)

(test-eq "smith-fletcher-not-adjacent"
  (smith-fletcher-not-adjacent '(m f s c b)) #f)
(test-eq "smith-fletcher-not-adjacent"
  (smith-fletcher-not-adjacent '(m c b s f)) #f)

(test-eq "fletcher-cooper-not-adjacent"
  (fletcher-cooper-not-adjacent '(m f c s b)) #f)
(test-eq "fletcher-cooper-not-adjacent"
  (fletcher-cooper-not-adjacent '(m c b s f)) #t)

(test-eq "fletcher-cooper-not-adjacent"
  (fletcher-cooper-not-adjacent '(f b c s m)) #t)

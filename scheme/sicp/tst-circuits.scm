;;;; tests for circuits.scm
(load-from-path "test-framework.scm")
(load-from-path "lib.scm")
(load-from-path "circuits.scm")

(set-test-section! "logic operators: and")

(test-eq "1" (logical-and 0 0) 0)
(test-eq "2" (logical-and 0 1) 0)
(test-eq "3" (logical-and 1 0) 0)
(test-eq "4" (logical-and 1 1) 1)

(set-test-section! "logic operators: or")

(test-eq "1" (logical-or 0 0) 0)
(test-eq "2" (logical-or 0 1) 1)
(test-eq "3" (logical-or 1 0) 1)
(test-eq "4" (logical-or 1 1) 1)

(set-test-section! "logic operators: xor")

(test-eq "1" (xor 0 0) 0)
(test-eq "2" (xor 0 1) 1)
(test-eq "3" (xor 1 0) 1)
(test-eq "4" (xor 1 1) 0)

(set-test-section! "logic operators: not")

(test-eq "1" (logical-not 0) 1)
(test-eq "2" (logical-not 1) 0)


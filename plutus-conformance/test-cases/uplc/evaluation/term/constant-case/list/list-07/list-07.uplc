-- case on empty list with two incorrectly typed branch
(program 1.1.0
    (case (con (list integer) [1, 2, 3, 4]) (lam x (lam xs x)) (con integer -1))
)

; A nonsense domain for testing parsing.
(define (domain domain-features)
  (:requirements :adl)

  (:types 
    foo bar - baz 
    atype btype - (either blab blob)
    ctype)

  (:constants
    f1 f2 - foo
    a1 b1 b2 - (either atype btype)
    c1 c2)

  (:predicates
    (p1)
    (p2 ?v1 ?v2)
    (p3 ?v1 - (either foo atype ctype) ?v2)
    (p4 ?v1 ?v2 - foo)
    (p5 ?v1 - ctype))

  (:derived (p1) (forall (?v - ctype) (p2 ?v ?v)))
  (:derived (p2 ?v1 ?v1) (p5 ?v1))

  (:action a1
   :precondition ()
   :effect ())

)

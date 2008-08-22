
(/VERSIONCHECK 2) 

(DEFPARAMETER |EuclideanDomain;AL| 'NIL) 

(DEFUN |EuclideanDomain| ()
  (LET (#:G1411)
    (COND
      (|EuclideanDomain;AL|)
      (T (SETQ |EuclideanDomain;AL| (|EuclideanDomain;|)))))) 

(DEFUN |EuclideanDomain;| ()
  (PROG (#0=#:G1409)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|PrincipalIdealDomain|)
                           (|mkCategory| '|domain|
                               '(((|sizeLess?| ((|Boolean|) $ $)) T)
                                 ((|euclideanSize|
                                      ((|NonNegativeInteger|) $))
                                  T)
                                 ((|divide|
                                      ((|Record| (|:| |quotient| $)
                                        (|:| |remainder| $))
                                       $ $))
                                  T)
                                 ((|quo| ($ $ $)) T)
                                 ((|rem| ($ $ $)) T)
                                 ((|extendedEuclidean|
                                      ((|Record| (|:| |coef1| $)
                                        (|:| |coef2| $)
                                        (|:| |generator| $))
                                       $ $))
                                  T)
                                 ((|extendedEuclidean|
                                      ((|Union|
                                        (|Record| (|:| |coef1| $)
                                         (|:| |coef2| $))
                                        "failed")
                                       $ $ $))
                                  T)
                                 ((|multiEuclidean|
                                      ((|Union| (|List| $) "failed")
                                       (|List| $) $))
                                  T))
                               NIL
                               '((|List| $) (|NonNegativeInteger|)
                                 (|Boolean|))
                               NIL))
                   |EuclideanDomain|)
        (SETELT #0# 0 '(|EuclideanDomain|)))))) 

(SETQ |$CategoryFrame|
      (|put| '|EuclideanDomain| '|isCategory| T
             (|addModemap| '|EuclideanDomain| '(|EuclideanDomain|)
                 '((|Category|)) T '|EuclideanDomain| |$CategoryFrame|))) 

(MAKEPROP '|EuclideanDomain| 'NILADIC T) 

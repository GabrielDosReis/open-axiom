
(/VERSIONCHECK 2) 

(DEFPARAMETER |IntegralDomain;AL| 'NIL) 

(DEFUN |IntegralDomain;| ()
  (PROG (#0=#:G1402)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|CommutativeRing|) (|Algebra| '$)
                           (|EntireRing|)
                           (|mkCategory| '|domain|
                               '(((|exquo| ((|Union| $ "failed") $ $))
                                  T)
                                 ((|unitNormal|
                                      ((|Record| (|:| |unit| $)
                                        (|:| |canonical| $)
                                        (|:| |associate| $))
                                       $))
                                  T)
                                 ((|unitCanonical| ($ $)) T)
                                 ((|associates?| ((|Boolean|) $ $)) T)
                                 ((|unit?| ((|Boolean|) $)) T))
                               NIL '((|Boolean|)) NIL))
                   |IntegralDomain|)
        (|setShellEntry| #0# 0 '(|IntegralDomain|)))))) 

(DEFUN |IntegralDomain| ()
  (LET ()
    (COND
      (|IntegralDomain;AL|)
      (T (SETQ |IntegralDomain;AL| (|IntegralDomain;|)))))) 

(MAKEPROP '|IntegralDomain| 'NILADIC T) 

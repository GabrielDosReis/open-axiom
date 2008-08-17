
(/VERSIONCHECK 2) 

(DEFPARAMETER |IntegralDomain;AL| 'NIL) 

(DEFUN |IntegralDomain| ()
  (LET (#:G1402)
    (COND
      (|IntegralDomain;AL|)
      (T (SETQ |IntegralDomain;AL| (|IntegralDomain;|)))))) 

(DEFUN |IntegralDomain;| ()
  (PROG (#0=#:G1400)
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
        (SETELT #0# 0 '(|IntegralDomain|)))))) 

(SETQ |$CategoryFrame|
      (|put| '|IntegralDomain| '|isCategory| T
             (|addModemap| '|IntegralDomain| '(|IntegralDomain|)
                 '((|Category|)) T '|IntegralDomain| |$CategoryFrame|))) 

(MAKEPROP '|IntegralDomain| 'NILADIC T) 

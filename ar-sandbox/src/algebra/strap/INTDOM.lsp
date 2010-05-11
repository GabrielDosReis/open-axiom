
(/VERSIONCHECK 2) 

(DEFPARAMETER |IntegralDomain;AL| 'NIL) 

(DEFUN |IntegralDomain;| ()
  (LET ((#0=#:G1402
            (|Join| (|CommutativeRing|) (|Algebra| '$) (|EntireRing|)
                    (|mkCategory| '|domain|
                        '(((|exquo| ((|Union| $ "failed") $ $)) T)
                          ((|unitNormal|
                               ((|Record| (|:| |unit| $)
                                    (|:| |canonical| $)
                                    (|:| |associate| $))
                                $))
                           T)
                          ((|unitCanonical| ($ $)) T)
                          ((|associates?| ((|Boolean|) $ $)) T)
                          ((|unit?| ((|Boolean|) $)) T))
                        NIL '((|Boolean|)) NIL))))
    (|setShellEntry| #0# 0 '(|IntegralDomain|))
    #0#)) 

(DEFUN |IntegralDomain| ()
  (COND
    (|IntegralDomain;AL|)
    (T (SETQ |IntegralDomain;AL| (|IntegralDomain;|))))) 

(MAKEPROP '|IntegralDomain| 'NILADIC T) 

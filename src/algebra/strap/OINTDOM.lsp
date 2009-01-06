
(/VERSIONCHECK 2) 

(DEFPARAMETER |OrderedIntegralDomain;AL| 'NIL) 

(DEFUN |OrderedIntegralDomain;| ()
  (PROG (#0=#:G1398)
    (RETURN
      (PROG1 (LETT #0# (|Join| (|IntegralDomain|) (|OrderedRing|))
                   |OrderedIntegralDomain|)
        (|setShellEntry| #0# 0 '(|OrderedIntegralDomain|)))))) 

(DEFUN |OrderedIntegralDomain| ()
  (LET ()
    (COND
      (|OrderedIntegralDomain;AL|)
      (T (SETQ |OrderedIntegralDomain;AL| (|OrderedIntegralDomain;|)))))) 

(MAKEPROP '|OrderedIntegralDomain| 'NILADIC T) 

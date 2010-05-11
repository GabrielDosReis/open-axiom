
(/VERSIONCHECK 2) 

(DEFPARAMETER |OrderedIntegralDomain;AL| 'NIL) 

(DEFUN |OrderedIntegralDomain;| ()
  (LET ((#0=#:G1396 (|Join| (|IntegralDomain|) (|OrderedRing|))))
    (|setShellEntry| #0# 0 '(|OrderedIntegralDomain|))
    #0#)) 

(DEFUN |OrderedIntegralDomain| ()
  (COND
    (|OrderedIntegralDomain;AL|)
    (T (SETQ |OrderedIntegralDomain;AL| (|OrderedIntegralDomain;|))))) 

(MAKEPROP '|OrderedIntegralDomain| 'NILADIC T) 

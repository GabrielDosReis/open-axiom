
(/VERSIONCHECK 2) 

(DEFPARAMETER |OrderedIntegralDomain;AL| 'NIL) 

(DEFUN |OrderedIntegralDomain| ()
  (LET (#:G1396)
    (COND
      (|OrderedIntegralDomain;AL|)
      (T (SETQ |OrderedIntegralDomain;AL| (|OrderedIntegralDomain;|)))))) 

(DEFUN |OrderedIntegralDomain;| ()
  (PROG (#0=#:G1394)
    (RETURN
      (PROG1 (LETT #0# (|Join| (|IntegralDomain|) (|OrderedRing|))
                   |OrderedIntegralDomain|)
        (SETELT #0# 0 '(|OrderedIntegralDomain|)))))) 

(SETQ |$CategoryFrame|
      (|put| '|OrderedIntegralDomain| '|isCategory| T
             (|addModemap| '|OrderedIntegralDomain|
                 '(|OrderedIntegralDomain|) '((|Category|)) T
                 '|OrderedIntegralDomain| |$CategoryFrame|))) 

(MAKEPROP '|OrderedIntegralDomain| 'NILADIC T) 

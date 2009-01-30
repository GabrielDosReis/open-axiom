
(/VERSIONCHECK 2) 

(DEFPARAMETER |CommutativeRing;AL| 'NIL) 

(DEFUN |CommutativeRing| ()
  (LET (#:G1396)
    (COND
      (|CommutativeRing;AL|)
      (T (SETQ |CommutativeRing;AL| (|CommutativeRing;|)))))) 

(DEFUN |CommutativeRing;| ()
  (PROG (#0=#:G1394)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|Ring|) (|BiModule| '$ '$)
                           (|mkCategory| '|package| NIL
                               '(((|commutative| "*") T)) 'NIL NIL))
                   |CommutativeRing|)
        (SETELT #0# 0 '(|CommutativeRing|)))))) 

(SETQ |$CategoryFrame|
      (|put| '|CommutativeRing| '|isCategory| T
             (|addModemap| '|CommutativeRing| '(|CommutativeRing|)
                 '((|Category|)) T '|CommutativeRing| |$CategoryFrame|))) 

(MAKEPROP '|CommutativeRing| 'NILADIC T) 

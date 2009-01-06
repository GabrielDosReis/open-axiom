
(/VERSIONCHECK 2) 

(DEFPARAMETER |CommutativeRing;AL| 'NIL) 

(DEFUN |CommutativeRing;| ()
  (PROG (#0=#:G1398)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|Ring|) (|BiModule| '$ '$)
                           (|mkCategory| '|package| NIL
                               '(((|commutative| "*") T)) 'NIL NIL))
                   |CommutativeRing|)
        (|setShellEntry| #0# 0 '(|CommutativeRing|)))))) 

(DEFUN |CommutativeRing| ()
  (LET ()
    (COND
      (|CommutativeRing;AL|)
      (T (SETQ |CommutativeRing;AL| (|CommutativeRing;|)))))) 

(MAKEPROP '|CommutativeRing| 'NILADIC T) 

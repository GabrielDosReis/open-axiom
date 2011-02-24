
(/VERSIONCHECK 2) 

(DEFPARAMETER |CommutativeRing;AL| 'NIL) 

(DEFUN |CommutativeRing;| ()
  (LET ((#0=#:G1372
            (|Join| (|Ring|) (|BiModule| '$ '$)
                    (|mkCategory| '|package| NIL
                        '(((|commutative| "*") T)) 'NIL NIL))))
    (SETF (|shellEntry| #0# 0) '(|CommutativeRing|))
    #0#)) 

(DEFUN |CommutativeRing| ()
  (COND
    (|CommutativeRing;AL|)
    (T (SETQ |CommutativeRing;AL| (|CommutativeRing;|))))) 

(MAKEPROP '|CommutativeRing| 'NILADIC T) 

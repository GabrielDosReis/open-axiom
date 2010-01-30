
(/VERSIONCHECK 2) 

(DEFPARAMETER |FloatingPointSystem;AL| 'NIL) 

(DEFUN |FloatingPointSystem;| ()
  (PROG (#0=#:G1396)
    (RETURN
      (PROG1 (LETT #0#
                   (|Join| (|RealNumberSystem|)
                           (|mkCategory| '|domain|
                               '(((|float| ($ (|Integer|) (|Integer|)))
                                  T)
                                 ((|float| ($ (|Integer|) (|Integer|)
                                            (|PositiveInteger|)))
                                  T)
                                 ((|order| ((|Integer|) $)) T)
                                 ((|base| ((|PositiveInteger|))) T)
                                 ((|exponent| ((|Integer|) $)) T)
                                 ((|mantissa| ((|Integer|) $)) T)
                                 ((|bits| ((|PositiveInteger|))) T)
                                 ((|digits| ((|PositiveInteger|))) T)
                                 ((|precision| ((|PositiveInteger|)))
                                  T)
                                 ((|bits| ((|PositiveInteger|)
                                           (|PositiveInteger|)))
                                  (|has| $
                                         (ATTRIBUTE
                                          |arbitraryPrecision|)))
                                 ((|digits|
                                      ((|PositiveInteger|)
                                       (|PositiveInteger|)))
                                  (|has| $
                                         (ATTRIBUTE
                                          |arbitraryPrecision|)))
                                 ((|precision|
                                      ((|PositiveInteger|)
                                       (|PositiveInteger|)))
                                  (|has| $
                                         (ATTRIBUTE
                                          |arbitraryPrecision|)))
                                 ((|increasePrecision|
                                      ((|PositiveInteger|) (|Integer|)))
                                  (|has| $
                                         (ATTRIBUTE
                                          |arbitraryPrecision|)))
                                 ((|decreasePrecision|
                                      ((|PositiveInteger|) (|Integer|)))
                                  (|has| $
                                         (ATTRIBUTE
                                          |arbitraryPrecision|)))
                                 ((|min| ($))
                                  (AND (|not|
                                        (|has| $
                                         (ATTRIBUTE
                                          |arbitraryPrecision|)))
                                       (|not|
                                        (|has| $
                                         (ATTRIBUTE
                                          |arbitraryExponent|)))))
                                 ((|max| ($))
                                  (AND (|not|
                                        (|has| $
                                         (ATTRIBUTE
                                          |arbitraryPrecision|)))
                                       (|not|
                                        (|has| $
                                         (ATTRIBUTE
                                          |arbitraryExponent|))))))
                               '((|approximate| T))
                               '((|PositiveInteger|) (|Integer|)) NIL))
                   |FloatingPointSystem|)
        (|setShellEntry| #0# 0 '(|FloatingPointSystem|)))))) 

(DEFUN |FloatingPointSystem| ()
  (LET ()
    (COND
      (|FloatingPointSystem;AL|)
      (T (SETQ |FloatingPointSystem;AL| (|FloatingPointSystem;|)))))) 

(MAKEPROP '|FloatingPointSystem| 'NILADIC T) 

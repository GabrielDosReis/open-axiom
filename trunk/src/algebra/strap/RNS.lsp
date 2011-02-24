
(/VERSIONCHECK 2) 

(DEFPARAMETER |RealNumberSystem;AL| 'NIL) 

(DEFUN |RealNumberSystem;| ()
  (LET ((#0=#:G1381
            (|sublisV|
                (PAIR '(#1=#:G1377 #2=#:G1378 #3=#:G1379 #4=#:G1380)
                      '((|Integer|) (|Fraction| (|Integer|))
                        (|Pattern| (|Float|)) (|Float|)))
                (|Join| (|Field|) (|OrderedRing|) (|RealConstant|)
                        (|RetractableTo| '#1#) (|RetractableTo| '#2#)
                        (|RadicalCategory|) (|ConvertibleTo| '#3#)
                        (|PatternMatchable| '#4#)
                        (|CharacteristicZero|)
                        (|mkCategory| '|domain|
                            '(((|norm| ($ $)) T) ((|ceiling| ($ $)) T)
                              ((|floor| ($ $)) T)
                              ((|wholePart| ((|Integer|) $)) T)
                              ((|fractionPart| ($ $)) T)
                              ((|truncate| ($ $)) T)
                              ((|round| ($ $)) T) ((|abs| ($ $)) T))
                            NIL '((|Integer|)) NIL)))))
    (SETF (|shellEntry| #0# 0) '(|RealNumberSystem|))
    #0#)) 

(DEFUN |RealNumberSystem| ()
  (COND
    (|RealNumberSystem;AL|)
    (T (SETQ |RealNumberSystem;AL| (|RealNumberSystem;|))))) 

(MAKEPROP '|RealNumberSystem| 'NILADIC T) 

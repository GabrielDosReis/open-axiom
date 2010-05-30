
(/VERSIONCHECK 2) 

(DEFPARAMETER |RealNumberSystem;AL| 'NIL) 

(DEFUN |RealNumberSystem;| ()
  (LET ((#0=#:G1406
            (|sublisV|
                (PAIR '(#1=#:G1402 #2=#:G1403 #3=#:G1404 #4=#:G1405)
                      (LIST '(|Integer|) '(|Fraction| (|Integer|))
                            '(|Pattern| (|Float|)) '(|Float|)))
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
    (|setShellEntry| #0# 0 '(|RealNumberSystem|))
    #0#)) 

(DEFUN |RealNumberSystem| ()
  (COND
    (|RealNumberSystem;AL|)
    (T (SETQ |RealNumberSystem;AL| (|RealNumberSystem;|))))) 

(MAKEPROP '|RealNumberSystem| 'NILADIC T) 

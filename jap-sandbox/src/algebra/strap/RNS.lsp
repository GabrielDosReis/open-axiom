
(/VERSIONCHECK 2) 

(DEFPARAMETER |RealNumberSystem;AL| 'NIL) 

(DEFUN |RealNumberSystem;| ()
  (PROG (#0=#:G1407)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(#1=#:G1403 #2=#:G1404 #3=#:G1405
                                  #4=#:G1406)
                             (LIST '(|Integer|)
                                   '(|Fraction| (|Integer|))
                                   '(|Pattern| (|Float|)) '(|Float|)))
                       (|Join| (|Field|) (|OrderedRing|)
                               (|RealConstant|) (|RetractableTo| '#1#)
                               (|RetractableTo| '#2#)
                               (|RadicalCategory|)
                               (|ConvertibleTo| '#3#)
                               (|PatternMatchable| '#4#)
                               (|CharacteristicZero|)
                               (|mkCategory| '|domain|
                                   '(((|norm| ($ $)) T)
                                     ((|ceiling| ($ $)) T)
                                     ((|floor| ($ $)) T)
                                     ((|wholePart| ((|Integer|) $)) T)
                                     ((|fractionPart| ($ $)) T)
                                     ((|truncate| ($ $)) T)
                                     ((|round| ($ $)) T)
                                     ((|abs| ($ $)) T))
                                   NIL '((|Integer|)) NIL)))
                   |RealNumberSystem|)
        (|setShellEntry| #0# 0 '(|RealNumberSystem|)))))) 

(DEFUN |RealNumberSystem| ()
  (LET ()
    (COND
      (|RealNumberSystem;AL|)
      (T (SETQ |RealNumberSystem;AL| (|RealNumberSystem;|)))))) 

(MAKEPROP '|RealNumberSystem| 'NILADIC T) 

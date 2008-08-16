
(/VERSIONCHECK 2) 

(DEFPARAMETER |RealNumberSystem;AL| 'NIL) 

(DEFUN |RealNumberSystem| ()
  (LET (#:G1396)
    (COND
      (|RealNumberSystem;AL|)
      (T (SETQ |RealNumberSystem;AL| (|RealNumberSystem;|)))))) 

(DEFUN |RealNumberSystem;| ()
  (PROG (#0=#:G1394)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(#1=#:G1390 #2=#:G1391 #3=#:G1392
                                  #4=#:G1393)
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
        (SETELT #0# 0 '(|RealNumberSystem|)))))) 

(MAKEPROP '|RealNumberSystem| 'NILADIC T) 

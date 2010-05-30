
(/VERSIONCHECK 2) 

(DEFPARAMETER |UnaryRecursiveAggregate;CAT| 'NIL) 

(DEFPARAMETER |UnaryRecursiveAggregate;AL| 'NIL) 

(DEFUN |UnaryRecursiveAggregate;| (|t#1|)
  (LET ((#0=#:G1425
            (|sublisV| (PAIR '(|t#1|) (LIST (|devaluate| |t#1|)))
                (COND
                  (|UnaryRecursiveAggregate;CAT|)
                  ('T
                   (LETT |UnaryRecursiveAggregate;CAT|
                         (|Join| (|RecursiveAggregate| '|t#1|)
                                 (|mkCategory| '|domain|
                                     '(((|concat| ($ $ $)) T)
                                       ((|concat| ($ |t#1| $)) T)
                                       ((|first| (|t#1| $)) T)
                                       ((|elt| (|t#1| $ "first")) T)
                                       ((|first|
                                         ($ $ (|NonNegativeInteger|)))
                                        T)
                                       ((|rest| ($ $)) T)
                                       ((|elt| ($ $ "rest")) T)
                                       ((|rest|
                                         ($ $ (|NonNegativeInteger|)))
                                        T)
                                       ((|last| (|t#1| $)) T)
                                       ((|elt| (|t#1| $ "last")) T)
                                       ((|last|
                                         ($ $ (|NonNegativeInteger|)))
                                        T)
                                       ((|tail| ($ $)) T)
                                       ((|second| (|t#1| $)) T)
                                       ((|third| (|t#1| $)) T)
                                       ((|cycleEntry| ($ $)) T)
                                       ((|cycleLength|
                                         ((|NonNegativeInteger|) $))
                                        T)
                                       ((|cycleTail| ($ $)) T)
                                       ((|concat!| ($ $ $))
                                        (|has| $
                                         (ATTRIBUTE |shallowlyMutable|)))
                                       ((|concat!| ($ $ |t#1|))
                                        (|has| $
                                         (ATTRIBUTE |shallowlyMutable|)))
                                       ((|cycleSplit!| ($ $))
                                        (|has| $
                                         (ATTRIBUTE |shallowlyMutable|)))
                                       ((|setfirst!| (|t#1| $ |t#1|))
                                        (|has| $
                                         (ATTRIBUTE |shallowlyMutable|)))
                                       ((|setelt|
                                         (|t#1| $ "first" |t#1|))
                                        (|has| $
                                         (ATTRIBUTE |shallowlyMutable|)))
                                       ((|setrest!| ($ $ $))
                                        (|has| $
                                         (ATTRIBUTE |shallowlyMutable|)))
                                       ((|setelt| ($ $ "rest" $))
                                        (|has| $
                                         (ATTRIBUTE |shallowlyMutable|)))
                                       ((|setlast!| (|t#1| $ |t#1|))
                                        (|has| $
                                         (ATTRIBUTE |shallowlyMutable|)))
                                       ((|setelt|
                                         (|t#1| $ "last" |t#1|))
                                        (|has| $
                                         (ATTRIBUTE |shallowlyMutable|)))
                                       ((|split!| ($ $ (|Integer|)))
                                        (|has| $
                                         (ATTRIBUTE |shallowlyMutable|))))
                                     NIL
                                     '((|Integer|)
                                       (|NonNegativeInteger|))
                                     NIL))
                         |UnaryRecursiveAggregate|))))))
    (|setShellEntry| #0# 0
        (LIST '|UnaryRecursiveAggregate| (|devaluate| |t#1|)))
    #0#)) 

(DEFUN |UnaryRecursiveAggregate| (#0=#:G1426)
  (LET ((#1=#:G1427
            (|assoc| (|devaluate| #0#) |UnaryRecursiveAggregate;AL|)))
    (COND
      (#1# (CDR #1#))
      (T (PROGN
           (SETQ #1# (|UnaryRecursiveAggregate;| #0#))
           (SETQ |UnaryRecursiveAggregate;AL|
                 (|cons5| (CONS (|devaluate| #0#) #1#)
                          |UnaryRecursiveAggregate;AL|))
           #1#))))) 

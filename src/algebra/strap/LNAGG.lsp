
(/VERSIONCHECK 2) 

(DEFPARAMETER |LinearAggregate;CAT| 'NIL) 

(DEFPARAMETER |LinearAggregate;AL| 'NIL) 

(DEFUN |LinearAggregate;| (|t#1|)
  (LET ((#0=#:G1400
            (|sublisV| (PAIR '(|t#1|) (LIST (|devaluate| |t#1|)))
                (|sublisV|
                    (PAIR '(#1=#:G1398 #2=#:G1399)
                          (LIST '(|Integer|)
                                '(|UniversalSegment| (|Integer|))))
                    (COND
                      (|LinearAggregate;CAT|)
                      (T (SETQ |LinearAggregate;CAT|
                               (|Join| (|IndexedAggregate| '#1# '|t#1|)
                                       (|Collection| '|t#1|)
                                       (|Eltable| '#2# '$)
                                       (|mkCategory| '|domain|
                                        '(((|new|
                                            ($ (|NonNegativeInteger|)
                                             |t#1|))
                                           T)
                                          ((|concat| ($ $ |t#1|)) T)
                                          ((|concat| ($ |t#1| $)) T)
                                          ((|concat| ($ $ $)) T)
                                          ((|concat| ($ (|List| $))) T)
                                          ((|map|
                                            ($
                                             (|Mapping| |t#1| |t#1|
                                              |t#1|)
                                             $ $))
                                           T)
                                          ((|delete| ($ $ (|Integer|)))
                                           T)
                                          ((|delete|
                                            ($ $
                                             (|UniversalSegment|
                                              (|Integer|))))
                                           T)
                                          ((|insert|
                                            ($ |t#1| $ (|Integer|)))
                                           T)
                                          ((|insert|
                                            ($ $ $ (|Integer|)))
                                           T)
                                          ((|setelt|
                                            (|t#1| $
                                             (|UniversalSegment|
                                              (|Integer|))
                                             |t#1|))
                                           (|has| $
                                            (ATTRIBUTE
                                             |shallowlyMutable|))))
                                        NIL
                                        '((|UniversalSegment|
                                           (|Integer|))
                                          (|Integer|) (|List| $)
                                          (|NonNegativeInteger|))
                                        NIL)))))))))
    (|setShellEntry| #0# 0
        (LIST '|LinearAggregate| (|devaluate| |t#1|)))
    #0#)) 

(DEFUN |LinearAggregate| (#0=#:G1401)
  (LET ((#1=#:G1402 (|assoc| (|devaluate| #0#) |LinearAggregate;AL|)))
    (COND
      (#1# (CDR #1#))
      (T (PROGN
           (SETQ #1# (|LinearAggregate;| #0#))
           (SETQ |LinearAggregate;AL|
                 (|cons5| (CONS (|devaluate| #0#) #1#)
                          |LinearAggregate;AL|))
           #1#))))) 

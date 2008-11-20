
(/VERSIONCHECK 2) 

(DEFPARAMETER |LinearAggregate;CAT| 'NIL) 

(DEFPARAMETER |LinearAggregate;AL| 'NIL) 

(DEFUN |LinearAggregate;| (|t#1|)
  (PROG (#0=#:G1399)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(|t#1|) (LIST (|devaluate| |t#1|)))
                       (|sublisV|
                           (PAIR '(#1=#:G1398) (LIST '(|Integer|)))
                           (COND
                             (|LinearAggregate;CAT|)
                             ('T
                              (LETT |LinearAggregate;CAT|
                                    (|Join|
                                     (|IndexedAggregate| '#1# '|t#1|)
                                     (|Collection| '|t#1|)
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
                                        ((|elt|
                                          ($ $
                                           (|UniversalSegment|
                                            (|Integer|))))
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
                                        ((|insert| ($ $ $ (|Integer|)))
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
                                      NIL))
                                    . #2=(|LinearAggregate|)))))) . #2#)
        (|setShellEntry| #0# 0
            (LIST '|LinearAggregate| (|devaluate| |t#1|))))))) 

(DEFUN |LinearAggregate| (#0=#:G1400)
  (LET (#1=#:G1401)
    (COND
      ((SETQ #1# (|assoc| (|devaluate| #0#) |LinearAggregate;AL|))
       (CDR #1#))
      (T (SETQ |LinearAggregate;AL|
               (|cons5| (CONS (|devaluate| #0#)
                              (SETQ #1# (|LinearAggregate;| #0#)))
                        |LinearAggregate;AL|))
         #1#)))) 

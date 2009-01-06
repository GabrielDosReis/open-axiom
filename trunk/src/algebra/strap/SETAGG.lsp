
(/VERSIONCHECK 2) 

(DEFPARAMETER |SetAggregate;CAT| 'NIL) 

(DEFPARAMETER |SetAggregate;AL| 'NIL) 

(DEFUN |SetAggregate;| (|t#1|)
  (PROG (#0=#:G1398)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(|t#1|) (LIST (|devaluate| |t#1|)))
                       (COND
                         (|SetAggregate;CAT|)
                         ('T
                          (LETT |SetAggregate;CAT|
                                (|Join| (|SetCategory|)
                                        (|Collection| '|t#1|)
                                        (|mkCategory| '|domain|
                                         '(((|part?| ((|Boolean|) $ $))
                                            T)
                                           ((|brace| ($)) T)
                                           ((|brace|
                                             ($ (|List| |t#1|)))
                                            T)
                                           ((|set| ($)) T)
                                           ((|set| ($ (|List| |t#1|)))
                                            T)
                                           ((|intersect| ($ $ $)) T)
                                           ((|difference| ($ $ $)) T)
                                           ((|difference| ($ $ |t#1|))
                                            T)
                                           ((|symmetricDifference|
                                             ($ $ $))
                                            T)
                                           ((|subset?|
                                             ((|Boolean|) $ $))
                                            T)
                                           ((|union| ($ $ $)) T)
                                           ((|union| ($ $ |t#1|)) T)
                                           ((|union| ($ |t#1| $)) T))
                                         '((|partiallyOrderedSet| T))
                                         '((|Boolean|) (|List| |t#1|))
                                         NIL))
                                . #1=(|SetAggregate|))))) . #1#)
        (|setShellEntry| #0# 0
            (LIST '|SetAggregate| (|devaluate| |t#1|))))))) 

(DEFUN |SetAggregate| (#0=#:G1399)
  (LET (#1=#:G1400)
    (COND
      ((SETQ #1# (|assoc| (|devaluate| #0#) |SetAggregate;AL|))
       (CDR #1#))
      (T (SETQ |SetAggregate;AL|
               (|cons5| (CONS (|devaluate| #0#)
                              (SETQ #1# (|SetAggregate;| #0#)))
                        |SetAggregate;AL|))
         #1#)))) 


(/VERSIONCHECK 2) 

(DEFPARAMETER |SetAggregate;CAT| 'NIL) 

(DEFPARAMETER |SetAggregate;AL| 'NIL) 

(DEFUN |SetAggregate;| (|t#1|)
  (LET ((#0=#:G1396
            (|sublisV| (PAIR '(|t#1|) (LIST (|devaluate| |t#1|)))
                (COND
                  (|SetAggregate;CAT|)
                  ('T
                   (LETT |SetAggregate;CAT|
                         (|Join| (|SetCategory|) (|Collection| '|t#1|)
                                 (|mkCategory| '|domain|
                                     '(((|part?| ((|Boolean|) $ $)) T)
                                       ((|brace| ($)) T)
                                       ((|brace| ($ (|List| |t#1|))) T)
                                       ((|set| ($)) T)
                                       ((|set| ($ (|List| |t#1|))) T)
                                       ((|intersect| ($ $ $)) T)
                                       ((|difference| ($ $ $)) T)
                                       ((|difference| ($ $ |t#1|)) T)
                                       ((|symmetricDifference| ($ $ $))
                                        T)
                                       ((|subset?| ((|Boolean|) $ $))
                                        T)
                                       ((|union| ($ $ $)) T)
                                       ((|union| ($ $ |t#1|)) T)
                                       ((|union| ($ |t#1| $)) T))
                                     '((|partiallyOrderedSet| T))
                                     '((|Boolean|) (|List| |t#1|)) NIL))
                         |SetAggregate|))))))
    (|setShellEntry| #0# 0 (LIST '|SetAggregate| (|devaluate| |t#1|)))
    #0#)) 

(DEFUN |SetAggregate| (#0=#:G1397)
  (LET ((#1=#:G1398 (|assoc| (|devaluate| #0#) |SetAggregate;AL|)))
    (COND
      (#1# (CDR #1#))
      (T (PROGN
           (SETQ #1# (|SetAggregate;| #0#))
           (SETQ |SetAggregate;AL|
                 (|cons5| (CONS (|devaluate| #0#) #1#)
                          |SetAggregate;AL|))
           #1#))))) 

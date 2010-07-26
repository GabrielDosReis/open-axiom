
(/VERSIONCHECK 2) 

(DEFPARAMETER |StreamAggregate;CAT| 'NIL) 

(DEFPARAMETER |StreamAggregate;AL| 'NIL) 

(DEFUN |StreamAggregate;| (|t#1|)
  (LET ((#0=#:G1404
            (|sublisV| (PAIR '(|t#1|) (LIST (|devaluate| |t#1|)))
                (COND
                  (|StreamAggregate;CAT|)
                  (T (SETQ |StreamAggregate;CAT|
                           (|Join| (|UnaryRecursiveAggregate| '|t#1|)
                                   (|LinearAggregate| '|t#1|)
                                   (|mkCategory| '|domain|
                                    '(((|explicitlyFinite?|
                                        ((|Boolean|) $))
                                       T)
                                      ((|possiblyInfinite?|
                                        ((|Boolean|) $))
                                       T))
                                    NIL '((|Boolean|)) NIL))))))))
    (|setShellEntry| #0# 0
        (LIST '|StreamAggregate| (|devaluate| |t#1|)))
    #0#)) 

(DEFUN |StreamAggregate| (#0=#:G1405)
  (LET ((#1=#:G1406 (|assoc| (|devaluate| #0#) |StreamAggregate;AL|)))
    (COND
      (#1# (CDR #1#))
      (T (PROGN
           (SETQ #1# (|StreamAggregate;| #0#))
           (SETQ |StreamAggregate;AL|
                 (|cons5| (CONS (|devaluate| #0#) #1#)
                          |StreamAggregate;AL|))
           #1#))))) 

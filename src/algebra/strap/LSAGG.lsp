
(/VERSIONCHECK 2) 

(DEFPARAMETER |ListAggregate;CAT| 'NIL) 

(DEFPARAMETER |ListAggregate;AL| 'NIL) 

(DEFUN |ListAggregate;| (|t#1|)
  (LET ((#0=#:G1429
            (|sublisV| (PAIR '(|t#1|) (LIST (|devaluate| |t#1|)))
                (COND
                  (|ListAggregate;CAT|)
                  ('T
                   (LETT |ListAggregate;CAT|
                         (|Join| (|StreamAggregate| '|t#1|)
                                 (|FiniteLinearAggregate| '|t#1|)
                                 (|ExtensibleLinearAggregate| '|t#1|)
                                 (|mkCategory| '|domain|
                                     '(((|list| ($ |t#1|)) T)) NIL 'NIL
                                     NIL))
                         |ListAggregate|))))))
    (|setShellEntry| #0# 0 (LIST '|ListAggregate| (|devaluate| |t#1|)))
    #0#)) 

(DEFUN |ListAggregate| (#0=#:G1430)
  (LET ((#1=#:G1431 (|assoc| (|devaluate| #0#) |ListAggregate;AL|)))
    (COND
      (#1# (CDR #1#))
      (T (PROGN
           (SETQ #1# (|ListAggregate;| #0#))
           (SETQ |ListAggregate;AL|
                 (|cons5| (CONS (|devaluate| #0#) #1#)
                          |ListAggregate;AL|))
           #1#))))) 


(/VERSIONCHECK 2) 

(DEFPARAMETER |ListAggregate;CAT| 'NIL) 

(DEFPARAMETER |ListAggregate;AL| 'NIL) 

(DEFUN |ListAggregate;| (|t#1|)
  (PROG (#0=#:G1431)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(|t#1|) (LIST (|devaluate| |t#1|)))
                       (COND
                         (|ListAggregate;CAT|)
                         ('T
                          (LETT |ListAggregate;CAT|
                                (|Join| (|StreamAggregate| '|t#1|)
                                        (|FiniteLinearAggregate|
                                         '|t#1|)
                                        (|ExtensibleLinearAggregate|
                                         '|t#1|)
                                        (|mkCategory| '|domain|
                                         '(((|list| ($ |t#1|)) T)) NIL
                                         'NIL NIL))
                                . #1=(|ListAggregate|))))) . #1#)
        (|setShellEntry| #0# 0
            (LIST '|ListAggregate| (|devaluate| |t#1|))))))) 

(DEFUN |ListAggregate| (#0=#:G1432)
  (LET (#1=#:G1433)
    (COND
      ((SETQ #1# (|assoc| (|devaluate| #0#) |ListAggregate;AL|))
       (CDR #1#))
      (T (SETQ |ListAggregate;AL|
               (|cons5| (CONS (|devaluate| #0#)
                              (SETQ #1# (|ListAggregate;| #0#)))
                        |ListAggregate;AL|))
         #1#)))) 

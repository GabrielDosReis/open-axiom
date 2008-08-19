
(/VERSIONCHECK 2) 

(DEFPARAMETER |ListAggregate;CAT| 'NIL) 

(DEFPARAMETER |ListAggregate;AL| 'NIL) 

(DEFUN |ListAggregate| (#0=#:G1428)
  (LET (#1=#:G1429)
    (COND
      ((SETQ #1# (|assoc| (|devaluate| #0#) |ListAggregate;AL|))
       (CDR #1#))
      (T (SETQ |ListAggregate;AL|
               (|cons5| (CONS (|devaluate| #0#)
                              (SETQ #1# (|ListAggregate;| #0#)))
                        |ListAggregate;AL|))
         #1#)))) 

(DEFUN |ListAggregate;| (|t#1|)
  (PROG (#0=#:G1427)
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
        (SETELT #0# 0 (LIST '|ListAggregate| (|devaluate| |t#1|))))))) 

(SETQ |$CategoryFrame|
      (|put| '|ListAggregate| '|isCategory| T
             (|addModemap| '|ListAggregate| '(|ListAggregate| |#1|)
                 '((|Category|) (|Type|)) T '|ListAggregate|
                 |$CategoryFrame|))) 

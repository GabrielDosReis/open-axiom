
(/VERSIONCHECK 2) 

(DEFPARAMETER |StreamAggregate;CAT| 'NIL) 

(DEFPARAMETER |StreamAggregate;AL| 'NIL) 

(DEFUN |StreamAggregate| (#0=#:G1402)
  (LET (#1=#:G1403)
    (COND
      ((SETQ #1# (|assoc| (|devaluate| #0#) |StreamAggregate;AL|))
       (CDR #1#))
      (T (SETQ |StreamAggregate;AL|
               (|cons5| (CONS (|devaluate| #0#)
                              (SETQ #1# (|StreamAggregate;| #0#)))
                        |StreamAggregate;AL|))
         #1#)))) 

(DEFUN |StreamAggregate;| (|t#1|)
  (PROG (#0=#:G1401)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(|t#1|) (LIST (|devaluate| |t#1|)))
                       (COND
                         (|StreamAggregate;CAT|)
                         ('T
                          (LETT |StreamAggregate;CAT|
                                (|Join| (|UnaryRecursiveAggregate|
                                         '|t#1|)
                                        (|LinearAggregate| '|t#1|)
                                        (|mkCategory| '|domain|
                                         '(((|explicitlyFinite?|
                                             ((|Boolean|) $))
                                            T)
                                           ((|possiblyInfinite?|
                                             ((|Boolean|) $))
                                            T))
                                         NIL '((|Boolean|)) NIL))
                                . #1=(|StreamAggregate|))))) . #1#)
        (SETELT #0# 0 (LIST '|StreamAggregate| (|devaluate| |t#1|))))))) 

(SETQ |$CategoryFrame|
      (|put| '|StreamAggregate| '|isCategory| T
             (|addModemap| '|StreamAggregate| '(|StreamAggregate| |#1|)
                 '((|Category|) (|Type|)) T '|StreamAggregate|
                 |$CategoryFrame|))) 

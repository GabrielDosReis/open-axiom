
(/VERSIONCHECK 2) 

(DEFPARAMETER |HomogeneousAggregate;CAT| 'NIL) 

(DEFPARAMETER |HomogeneousAggregate;AL| 'NIL) 

(DEFUN |HomogeneousAggregate;| (|t#1|)
  (PROG (#0=#:G1398)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(|t#1|) (LIST (|devaluate| |t#1|)))
                       (COND
                         (|HomogeneousAggregate;CAT|)
                         ('T
                          (LETT |HomogeneousAggregate;CAT|
                                (|Join| (|Aggregate|)
                                        (|mkCategory| '|domain|
                                         '(((|map|
                                             ($ (|Mapping| |t#1| |t#1|)
                                              $))
                                            T)
                                           ((|map!|
                                             ($ (|Mapping| |t#1| |t#1|)
                                              $))
                                            (|has| $
                                             (ATTRIBUTE
                                              |shallowlyMutable|)))
                                           ((|any?|
                                             ((|Boolean|)
                                              (|Mapping| (|Boolean|)
                                               |t#1|)
                                              $))
                                            (|has| $
                                             (ATTRIBUTE
                                              |finiteAggregate|)))
                                           ((|every?|
                                             ((|Boolean|)
                                              (|Mapping| (|Boolean|)
                                               |t#1|)
                                              $))
                                            (|has| $
                                             (ATTRIBUTE
                                              |finiteAggregate|)))
                                           ((|count|
                                             ((|NonNegativeInteger|)
                                              (|Mapping| (|Boolean|)
                                               |t#1|)
                                              $))
                                            (|has| $
                                             (ATTRIBUTE
                                              |finiteAggregate|)))
                                           ((|parts|
                                             ((|List| |t#1|) $))
                                            (|has| $
                                             (ATTRIBUTE
                                              |finiteAggregate|)))
                                           ((|members|
                                             ((|List| |t#1|) $))
                                            (|has| $
                                             (ATTRIBUTE
                                              |finiteAggregate|)))
                                           ((|count|
                                             ((|NonNegativeInteger|)
                                              |t#1| $))
                                            (AND
                                             (|has| |t#1|
                                              (|SetCategory|))
                                             (|has| $
                                              (ATTRIBUTE
                                               |finiteAggregate|))))
                                           ((|member?|
                                             ((|Boolean|) |t#1| $))
                                            (AND
                                             (|has| |t#1|
                                              (|SetCategory|))
                                             (|has| $
                                              (ATTRIBUTE
                                               |finiteAggregate|)))))
                                         '(((|CoercibleTo|
                                             (|OutputForm|))
                                            (|has| |t#1|
                                             (|CoercibleTo|
                                              (|OutputForm|))))
                                           ((|SetCategory|)
                                            (|has| |t#1|
                                             (|SetCategory|)))
                                           ((|Evalable| |t#1|)
                                            (AND
                                             (|has| |t#1|
                                              (|Evalable| |t#1|))
                                             (|has| |t#1|
                                              (|SetCategory|)))))
                                         '((|Boolean|)
                                           (|NonNegativeInteger|)
                                           (|List| |t#1|))
                                         NIL))
                                . #1=(|HomogeneousAggregate|))))) . #1#)
        (SETELT #0# 0
                (LIST '|HomogeneousAggregate| (|devaluate| |t#1|))))))) 

(DEFUN |HomogeneousAggregate| (#0=#:G1399)
  (LET (#1=#:G1400)
    (COND
      ((SETQ #1# (|assoc| (|devaluate| #0#) |HomogeneousAggregate;AL|))
       (CDR #1#))
      (T (SETQ |HomogeneousAggregate;AL|
               (|cons5| (CONS (|devaluate| #0#)
                              (SETQ #1# (|HomogeneousAggregate;| #0#)))
                        |HomogeneousAggregate;AL|))
         #1#)))) 

(SETQ |$CategoryFrame|
      (|put| '|HomogeneousAggregate| '|isCategory| T
             (|addModemap| '|HomogeneousAggregate|
                 '(|HomogeneousAggregate| |#1|)
                 '((|Category|) (|Type|)) T '|HomogeneousAggregate|
                 |$CategoryFrame|))) 

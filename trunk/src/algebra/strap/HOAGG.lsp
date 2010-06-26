
(/VERSIONCHECK 2) 

(DEFPARAMETER |HomogeneousAggregate;CAT| 'NIL) 

(DEFPARAMETER |HomogeneousAggregate;AL| 'NIL) 

(DEFUN |HomogeneousAggregate;| (|t#1|)
  (LET ((#0=#:G1398
            (|sublisV| (PAIR '(|t#1|) (LIST (|devaluate| |t#1|)))
                (COND
                  (|HomogeneousAggregate;CAT|)
                  ('T
                   (SETQ |HomogeneousAggregate;CAT|
                         (|Join| (|Aggregate|)
                                 (|mkCategory| '|domain|
                                     '(((|map|
                                         ($ (|Mapping| |t#1| |t#1|) $))
                                        T)
                                       ((|map!|
                                         ($ (|Mapping| |t#1| |t#1|) $))
                                        (|has| $
                                         (ATTRIBUTE |shallowlyMutable|)))
                                       ((|any?|
                                         ((|Boolean|)
                                          (|Mapping| (|Boolean|) |t#1|)
                                          $))
                                        (|has| $
                                         (ATTRIBUTE |finiteAggregate|)))
                                       ((|every?|
                                         ((|Boolean|)
                                          (|Mapping| (|Boolean|) |t#1|)
                                          $))
                                        (|has| $
                                         (ATTRIBUTE |finiteAggregate|)))
                                       ((|count|
                                         ((|NonNegativeInteger|)
                                          (|Mapping| (|Boolean|) |t#1|)
                                          $))
                                        (|has| $
                                         (ATTRIBUTE |finiteAggregate|)))
                                       ((|parts| ((|List| |t#1|) $))
                                        (|has| $
                                         (ATTRIBUTE |finiteAggregate|)))
                                       ((|members| ((|List| |t#1|) $))
                                        (|has| $
                                         (ATTRIBUTE |finiteAggregate|)))
                                       ((|count|
                                         ((|NonNegativeInteger|) |t#1|
                                          $))
                                        (AND
                                         (|has| |t#1| (|SetCategory|))
                                         (|has| $
                                          (ATTRIBUTE |finiteAggregate|))))
                                       ((|member?|
                                         ((|Boolean|) |t#1| $))
                                        (AND
                                         (|has| |t#1| (|SetCategory|))
                                         (|has| $
                                          (ATTRIBUTE |finiteAggregate|)))))
                                     '(((|CoercibleTo| (|OutputForm|))
                                        (|has| |t#1|
                                         (|CoercibleTo| (|OutputForm|))))
                                       ((|BasicType|)
                                        (|has| |t#1| (|BasicType|)))
                                       ((|SetCategory|)
                                        (|has| |t#1| (|SetCategory|)))
                                       ((|Evalable| |t#1|)
                                        (AND
                                         (|has| |t#1|
                                          (|Evalable| |t#1|))
                                         (|has| |t#1| (|SetCategory|)))))
                                     '((|Boolean|)
                                       (|NonNegativeInteger|)
                                       (|List| |t#1|))
                                     NIL))))))))
    (|setShellEntry| #0# 0
        (LIST '|HomogeneousAggregate| (|devaluate| |t#1|)))
    #0#)) 

(DEFUN |HomogeneousAggregate| (#0=#:G1399)
  (LET ((#1=#:G1400
            (|assoc| (|devaluate| #0#) |HomogeneousAggregate;AL|)))
    (COND
      (#1# (CDR #1#))
      (T (PROGN
           (SETQ #1# (|HomogeneousAggregate;| #0#))
           (SETQ |HomogeneousAggregate;AL|
                 (|cons5| (CONS (|devaluate| #0#) #1#)
                          |HomogeneousAggregate;AL|))
           #1#))))) 

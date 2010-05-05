
(/VERSIONCHECK 2) 

(DEFPARAMETER |AssociationListAggregate;CAT| 'NIL) 

(DEFPARAMETER |AssociationListAggregate;AL| 'NIL) 

(DEFUN |AssociationListAggregate;| (|t#1| |t#2|)
  (LET ((#0=#:G1398
            (|sublisV|
                (PAIR '(|t#1| |t#2|)
                      (LIST (|devaluate| |t#1|) (|devaluate| |t#2|)))
                (|sublisV|
                    (PAIR '(#1=#:G1397)
                          (LIST '(|Record| (|:| |key| |t#1|)
                                     (|:| |entry| |t#2|))))
                    (COND
                      (|AssociationListAggregate;CAT|)
                      ('T
                       (LETT |AssociationListAggregate;CAT|
                             (|Join| (|TableAggregate| '|t#1| '|t#2|)
                                     (|ListAggregate| '#1#)
                                     (|mkCategory| '|domain|
                                      '(((|assoc|
                                          ((|Union|
                                            (|Record| (|:| |key| |t#1|)
                                             (|:| |entry| |t#2|))
                                            "failed")
                                           |t#1| $))
                                         T))
                                      NIL 'NIL NIL))
                             |AssociationListAggregate|)))))))
    (|setShellEntry| #0# 0
        (LIST '|AssociationListAggregate| (|devaluate| |t#1|)
              (|devaluate| |t#2|)))
    #0#)) 

(DEFUN |AssociationListAggregate| (&REST #0=#:G1401 &AUX #1=#:G1399)
  (DSETQ #1# #0#)
  (LET ((#2=#:G1400
            (|assoc| (|devaluateList| #1#)
                     |AssociationListAggregate;AL|)))
    (COND
      (#2# (CDR #2#))
      (T (PROGN
           (SETQ #2# (APPLY #'|AssociationListAggregate;| #1#))
           (SETQ |AssociationListAggregate;AL|
                 (|cons5| (CONS (|devaluateList| #1#) #2#)
                          |AssociationListAggregate;AL|))
           #2#))))) 


(/VERSIONCHECK 2) 

(DEFPARAMETER |AssociationListAggregate;CAT| 'NIL) 

(DEFPARAMETER |AssociationListAggregate;AL| 'NIL) 

(DEFUN |AssociationListAggregate;| (|t#1| |t#2|)
  (PROG (#0=#:G1400)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(|t#1| |t#2|)
                             (LIST (|devaluate| |t#1|)
                                   (|devaluate| |t#2|)))
                       (|sublisV|
                           (PAIR '(#1=#:G1399)
                                 (LIST '(|Record| (|:| |key| |t#1|)
                                         (|:| |entry| |t#2|))))
                           (COND
                             (|AssociationListAggregate;CAT|)
                             ('T
                              (LETT |AssociationListAggregate;CAT|
                                    (|Join|
                                     (|TableAggregate| '|t#1| '|t#2|)
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
                                    . #2=(|AssociationListAggregate|)))))) . #2#)
        (|setShellEntry| #0# 0
            (LIST '|AssociationListAggregate| (|devaluate| |t#1|)
                  (|devaluate| |t#2|))))))) 

(DEFUN |AssociationListAggregate| (&REST #0=#:G1403 &AUX #1=#:G1401)
  (DSETQ #1# #0#)
  (LET (#2=#:G1402)
    (COND
      ((SETQ #2#
             (|assoc| (|devaluateList| #1#)
                      |AssociationListAggregate;AL|))
       (CDR #2#))
      (T (SETQ |AssociationListAggregate;AL|
               (|cons5| (CONS (|devaluateList| #1#)
                              (SETQ #2#
                                    (APPLY
                                     #'|AssociationListAggregate;| #1#)))
                        |AssociationListAggregate;AL|))
         #2#)))) 


(/VERSIONCHECK 2) 

(DEFPARAMETER |Collection;CAT| 'NIL) 

(DEFPARAMETER |Collection;AL| 'NIL) 

(DEFUN |Collection;| (|t#1|)
  (PROG (#0=#:G1397)
    (RETURN
      (PROG1 (LETT #0#
                   (|sublisV|
                       (PAIR '(|t#1|) (LIST (|devaluate| |t#1|)))
                       (COND
                         (|Collection;CAT|)
                         ('T
                          (LETT |Collection;CAT|
                                (|Join| (|HomogeneousAggregate| '|t#1|)
                                        (|mkCategory| '|domain|
                                         '(((|construct|
                                             ($ (|List| |t#1|)))
                                            T)
                                           ((|find|
                                             ((|Union| |t#1| "failed")
                                              (|Mapping| (|Boolean|)
                                               |t#1|)
                                              $))
                                            T)
                                           ((|reduce|
                                             (|t#1|
                                              (|Mapping| |t#1| |t#1|
                                               |t#1|)
                                              $))
                                            (|has| $
                                             (ATTRIBUTE
                                              |finiteAggregate|)))
                                           ((|reduce|
                                             (|t#1|
                                              (|Mapping| |t#1| |t#1|
                                               |t#1|)
                                              $ |t#1|))
                                            (|has| $
                                             (ATTRIBUTE
                                              |finiteAggregate|)))
                                           ((|remove|
                                             ($
                                              (|Mapping| (|Boolean|)
                                               |t#1|)
                                              $))
                                            (|has| $
                                             (ATTRIBUTE
                                              |finiteAggregate|)))
                                           ((|select|
                                             ($
                                              (|Mapping| (|Boolean|)
                                               |t#1|)
                                              $))
                                            (|has| $
                                             (ATTRIBUTE
                                              |finiteAggregate|)))
                                           ((|reduce|
                                             (|t#1|
                                              (|Mapping| |t#1| |t#1|
                                               |t#1|)
                                              $ |t#1| |t#1|))
                                            (AND
                                             (|has| |t#1|
                                              (|SetCategory|))
                                             (|has| $
                                              (ATTRIBUTE
                                               |finiteAggregate|))))
                                           ((|remove| ($ |t#1| $))
                                            (AND
                                             (|has| |t#1|
                                              (|SetCategory|))
                                             (|has| $
                                              (ATTRIBUTE
                                               |finiteAggregate|))))
                                           ((|removeDuplicates| ($ $))
                                            (AND
                                             (|has| |t#1|
                                              (|SetCategory|))
                                             (|has| $
                                              (ATTRIBUTE
                                               |finiteAggregate|)))))
                                         '(((|ConvertibleTo|
                                             (|InputForm|))
                                            (|has| |t#1|
                                             (|ConvertibleTo|
                                              (|InputForm|)))))
                                         '((|List| |t#1|)) NIL))
                                . #1=(|Collection|))))) . #1#)
        (|setShellEntry| #0# 0
            (LIST '|Collection| (|devaluate| |t#1|))))))) 

(DEFUN |Collection| (#0=#:G1398)
  (LET (#1=#:G1399)
    (COND
      ((SETQ #1# (|assoc| (|devaluate| #0#) |Collection;AL|))
       (CDR #1#))
      (T (SETQ |Collection;AL|
               (|cons5| (CONS (|devaluate| #0#)
                              (SETQ #1# (|Collection;| #0#)))
                        |Collection;AL|))
         #1#)))) 

-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2012, Gabriel Dos Reis.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--     - Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     - Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in
--       the documentation and/or other materials provided with the
--       distribution.
--
--     - Neither the name of The Numerical ALgorithms Group Ltd. nor the
--       names of its contributors may be used to endorse or promote products
--       derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
-- IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


-- not hooked in yet

-- BB parser tree to interpreter vectorized attributed trees.
-- Used to interface the BB parser
-- technology to the interpreter.  The input is a parseTree and the
-- output is an interpreter attributed tree.

$useParserSrcPos := true
$transferParserSrcPos := true

pf2Sexpr pf == packageTran (pf2Sex1)(pf)

pf2Atree pf ==
    (intUnsetQuiet)()

    $insideRule:        local := false
    $insideApplication: local := false
    $insideSEQ:         local := false

    -- we set the following because we will be using some things
    -- within pf2sex.boot and they are in the spadcomp package.

    ($insideRule):        local := false
    ($insideApplication): local := false
    ($insideSEQ):         local := false

    pf2Atree1 pf

pf2Atree1 pf ==
    -- some simple things that are really just S-expressions

    (pfNothing?)(pf) =>
        mkAtree1WithSrcPos(pf2Sexpr(pf), pf)
    (pfSymbol?) pf =>
        mkAtree1WithSrcPos(pf2Sexpr(pf), pf)
    (pfLiteral?)(pf) =>
        mkAtree1WithSrcPos(pf2Sexpr(pf), pf)
    (pfId?) pf =>
        mkAtree1WithSrcPos(pf2Sexpr(pf), pf)

    -- Now some compound forms

    (pfApplication?)(pf) =>
        pfApplication2Atree pf

    (pfTuple?)(pf) =>
        [mkAtreeNodeWithSrcPos("tuple",pf),
            :[pf2Atree1 x for x in (pf0TupleParts)(pf)]]

    (pfIf?)(pf) =>
        condPf      := (pfIfCond)(pf)
        condPart    := pf2Atree1 condPf
        thenPart    := pf2Atree1 (pfIfThen)(pf)
        elsePart    := pf2Atree1 (pfIfElse)(pf)
        ifPart      := mkAtreeNodeWithSrcPos("IF", pf)
        thenPart = "%noBranch" =>
            [ifPart, [mkAtreeNodeWithSrcPos("not", condPf), condPart],
                elsePart, thenPart]
        [ifPart, condPart, thenPart, elsePart]

    (pfTagged?)(pf) =>
        tag := (pfTaggedTag)(pf)
        tagPart :=
            (pfTuple?) tag =>
                ["tuple", :[pf2Sexpr(arg) for arg in (pf0TupleParts)(tag)]]
            pf2Sexpr(tag)
        [mkAtreeNodeWithSrcPos("Declare",pf), tagPart,
            pf2Sexpr((pfTaggedExpr)(pf))]

    (pfCoerceto?)(pf) =>
        [mkAtreeNodeWithSrcPos("COERCE",pf),
            pf2Atree1 (pfCoercetoExpr)(pf),
                pf2Sexpr((pfCoercetoType)(pf))]

    (pfPretend?)(pf) =>
        [mkAtreeNodeWithSrcPos("pretend",pf),
            pf2Atree1 (pfPretendExpr)(pf),
                pf2Sexpr((pfPretendType)(pf))]

    (pfFromdom?)(pf) =>
        op := packageTran (opTran)(pf2Sexpr((pfFromdomWhat)(pf)))
        if op = "braceFromCurly" then op := "SEQ"         -- ??

        op = 0 =>
            -- 0$Foo => Zero()$Foo
            [mkAtreeNodeWithSrcPos("Dollar",pf),
                pf2Sexpr((pfFromdomDomain)(pf)),
                    [mkAtreeNodeWithSrcPos("Zero",pf)]]
        op = 1 =>
            -- 1$Foo => One()$Foo
            [mkAtreeNodeWithSrcPos("Dollar",pf),
                pf2Sexpr((pfFromdomDomain)(pf)),
                    [mkAtreeNodeWithSrcPos("One",pf)]]
        integer? op =>
            -- n$Foo => n * One()$Foo
            [mkAtreeNodeWithSrcPos("*",pf),
                mkAtree1WithSrcPos(op,pf),
                    [mkAtreeNodeWithSrcPos("Dollar",pf),
                        pf2Sexpr((pfFromdomDomain)(pf)),
                            [mkAtreeNodeWithSrcPos("One",pf)]]]

        [mkAtreeNodeWithSrcPos("Dollar",pf),
            pf2Sexpr((pfFromdomDomain)(pf)),
                mkAtreeNodeWithSrcPos(op,pf)]

    (pfSequence?)(pf) =>
        pfSequence2Atree pf

    (pfExit?)(pf) =>
        $insideSEQ =>
            [mkAtreeNodeWithSrcPos("exit",pf),
                pf2Atree1 (pfExitCond)(pf),
                    pf2Atree1 (pfExitExpr)(pf)]
        [mkAtreeNodeWithSrcPos("IF",pf),
             pf2Atree1 (pfExitCond)(pf),
                pf2Atree1 (pfExitExpr)(pf), "%noBranch"]

    (pfLoop?)(pf) =>
        [mkAtreeNodeWithSrcPos("REPEAT",pf),
            :loopIters2Atree  (pf0LoopIterators)(pf)]

    (pfCollect?)(pf) =>
        pfCollect2Atree(pf)

    (pfForin?)(pf) =>
        ["IN", :[pf2Atree1 x for x in (pf0ForinLhs)(pf)],
            pf2Atree1 (pfForinWhole)(pf)]

    (pfWhile?)(pf) =>
        ["WHILE", pf2Atree1((pfWhileCond)(pf))]

    (pfSuchthat?)(pf) =>
        $insideRule = 'left =>
            keyedSystemError('"S2GE0017", ['"pf2Atree1: pfSuchThat"])
        ["SUCHTHAT", pf2Atree1 (pfSuchthatCond)(pf)]

    (pfDo?)(pf) =>
        pf2Atree1 (pfDoBody)(pf)

--  (pfTyped?)(pf) =>
--    type := pfTypedType pf
--    pfNothing? type => pf2Atree1 pfTypedId pf
--    [":", pf2Atree1 pfTypedId pf, pf2Atree1 pfTypedType pf]

    (pfAssign?)(pf) =>
        -- declarations on the lhs are broken out into another
        -- statement preceding the %LET of the id(s)
        lhsThings := (pf0AssignLhsItems)(pf)
        if #lhsThings = 1 and (pfTuple?)(first lhsThings) then
            lhsThings := (pf0TupleParts)(first lhsThings)
        decls := nil
        ids   := nil
        for x in lhsThings repeat
            (pfTagged?)(x) =>
                decls := [x, :decls]
                ids   := [(pfTaggedTag)(x), :ids]
            ids   := [x, :ids]
        idList := [pf2Atree1 x for x in reverse ids]
        if #idList ~= 1 then idList :=
            [mkAtreeNodeWithSrcPos("tuple",pf), :idList]
        else idList := first idList
        x := [mkAtreeNodeWithSrcPos("%LET",pf),
            idList, pf2Atree1 (pfAssignRhs)(pf)]
        decls =>
            [mkAtreeNodeWithSrcPos("SEQ",pf),
                :[pf2Atree1 decl for decl in reverse! decls], x]
        x

--  (pfDefinition?)(pf) =>
--      pfDefinition2Atree pf

--  (pfLambda?)(pf) =>
--    pfLambda2Atree pf
--  (pfRestrict?)(pf) =>
--    ["@", pf2Atree1 pfRestrictExpr pf, pf2Atree1 pfRestrictType pf]

    (pfFree?)(pf) =>
        [mkAtreeNodeWithSrcPos("free",pf),
            :[pf2Atree1 item for item in (pf0FreeItems)(pf)]]
    (pfLocal?)(pf) =>
        [mkAtreeNodeWithSrcPos("local",pf),
            :[pf2Atree1 item for item in (pf0LocalItems)(pf)]]

    (pfWrong?)(pf) =>
        spadThrow()

    -- next 3 are probably be handled in pfApplication2Atree

    (pfAnd?)(pf) =>
        [mkAtreeNodeWithSrcPos("and",pf),
            pf2Atree1 (pfAndLeft)(pf),
                pf2Atree1 (pfAndRight)(pf)]
    (pfOr?)(pf) =>
        [mkAtreeNodeWithSrcPos("or",pf),
            pf2Atree1 (pfOrLeft)(pf),
                pf2Atree1 (pfOrRight)(pf)]
    (pfNot?)(pf) =>
        [mkAtreeNodeWithSrcPos("not",pf),
            pf2Atree1 (pfNotArg)(pf)]

--  (pfNovalue?)(pf) =>
--    intSetQuiet()
--    ["SEQ", pf2Atree1 pfNovalueExpr pf]
--  (pfRule?)(pf) =>
--    pfRule2Atree pf

    (pfBreak?)(pf) =>
        [mkAtreeNodeWithSrcPos("break",pf), (pfBreakFrom)(pf)]

    (pfMacro?)(pf) =>
        tree := mkAtree1WithSrcPos('(void), pf)
        putValue(tree,objNewWrap(voidValue(),$Void))
        putModeSet(tree,[$Void])
        tree

    (pfReturn?)(pf) =>
        [mkAtreeNodeWithSrcPos("return",pf),
            pf2Atree1 (pfReturnExpr)(pf)]

    (pfIterate?)(pf) =>
        [mkAtreeNodeWithSrcPos("iterate",pf)]

--  (pfWhere?)(pf) =>
--    args := [pf2Atree1 p for p in pf0WhereContext pf]
--    #args = 1 =>
--      ["where", pf2Atree1 pfWhereExpr pf, :args]
--    ["where", pf2Atree1 pfWhereExpr pf, ["SEQ", :args]]

    mkAtree1WithSrcPos(pf2Sexpr(pf), pf)

--  keyedSystemError('"S2GE0017", ['"pf2Atree1"])
--

pfApplication2Atree pf ==
    $insideApplication:             local := true
    ($insideApplication): local := true

    opPf := (pfApplicationOp)(pf)
    op := packageTran ((opTran)(pfOp2Sex)(opPf))
    op = "->" =>
        args := (pf0TupleParts)((pfApplicationArg)(pf))
        if (pfTuple?)(first args) then
            typeList := [pf2Atree1 arg for arg in (pf0TupleParts)(first args)]
        else
            typeList := [pf2Atree1 first args]
        args := [pf2Atree1 second args, :typeList]
        [mkAtreeNodeWithSrcPos("Mapping", opPf), :args]

    (symEqual)(op, '":") and $insideRule = 'left =>
          [mkAtreeNodeWithSrcPos("multiple",opPf),
              pf2Atree (pfApplicationArg)(pf)]

    (symEqual)(op, '"?") and $insideRule = 'left =>
          [mkAtreeNodeWithSrcPos("optional",opPf),
              pf2Atree (pfApplicationArg)(pf)]

    args := (pfApplicationArg)(pf)

    (pfTuple?)(args) =>
--!     symEqual(op, '"|") and $insideRule = 'left =>
--!       pfSuchThat2Atree args
        argAtree := [pf2Atree1 arg for arg in (pf0TupleParts)(args)]

        (symEqual)(op, '">") =>
            [mkAtreeNodeWithSrcPos("<",opPf), :reverse(argAtree)]
        (symEqual)(op, '">=") =>
            [mkAtreeNodeWithSrcPos("not",opPf),
                [mkAtreeNodeWithSrcPos("<",opPf), :argAtree]]
        (symEqual)(op, '"<=") =>
            [mkAtreeNodeWithSrcPos("not",opPf),
                [mkAtreeNodeWithSrcPos("<",opPf), :reverse(argAtree)]]
        (symEqual)(op, '"AND") =>
            [mkAtreeNodeWithSrcPos("and",opPf), :argAtree]
        (symEqual)(op, '"OR") =>
            [mkAtreeNodeWithSrcPos("or",opPf), :argAtree]
        (symEqual) (op, '"Iterate") =>
            [mkAtreeNodeWithSrcPos("iterate",opPf)]
        (symEqual)(op, '"by") =>
            [mkAtreeNodeWithSrcPos("BY",opPf), :argAtree]
        (symEqual)(op, '"braceFromCurly") =>
            argAtree and getUnname first argAtree = "SEQ" => argAtree
            [mkAtreeNodeWithSrcPos("SEQ",opPf), :argAtree]
        op is [qt, realOp] and (symEqual)(qt, '"QUOTE") =>
            [mkAtreeNodeWithSrcPos("applyQuote",opPf),
                mkAtreeNodeWithSrcPos(op,opPf), :argAtree]
--!     val := (hasOptArgs?)(argSex) => [op, :val]
        -- handle package call
        (pfFromdom?)(opPf) =>
            opAtree := pf2Atree1 opPf
            [first opAtree, second opAtree, [third opAtree, :argAtree]]
        -- regular call
        [mkAtreeNodeWithSrcPos(op,opPf), :argAtree]

    op is [qt, realOp] and (symEqual)(qt, '"QUOTE") =>
       [mkAtreeNodeWithSrcPos("applyQuote",opPf),
           mkAtreeNodeWithSrcPos(op,opPf), pf2Atree1 args]
    (symEqual)(op, '"braceFromCurly") =>
        x := pf2Atree1 args
        x and getUnname x = "SEQ" => x
        [mkAtreeNodeWithSrcPos("SEQ",opPf), x]
    (symEqual)(op, '"by") =>
        [mkAtreeNodeWithSrcPos("BY",opPf), pf2Atree1 args]
    -- handle package call
    (pfFromdom?)(opPf) =>
        opAtree := pf2Atree1 opPf
        [first opAtree, second opAtree, [third opAtree, pf2Atree1 args]]
    -- regular call
    [mkAtreeNodeWithSrcPos(op,opPf), pf2Atree1 args]

--  pfDefinition2Atree pf ==
--  --! $insideApplication =>
--  --!     ["OPTARG", pf2Atree1 first pf0DefinitionLhsItems pf,
--  --!         pf2Atree1 pfDefinitionRhs pf]
--      idList := [pf2Atree1 x for x in (pf0DefinitionLhsItems)(pf)]
--      #idList ~= 1 =>
--          systemError '"lhs of definition must be a single item in the interpreter"
--      id := first idList
--      rhs := (pfDefinitionRhs)(pf)
--      [argList, :body] := pfLambdaTran rhs
--      ["DEF", (argList = 'id => id; [id, :argList]), :body]

--  pfLambdaTran pf ==
--    pfLambda? pf =>
--      argTypeList := nil
--      argList := nil
--      for arg in pf0LambdaArgs pf repeat
--        pfTyped? arg =>
--          argList := [pfCollectArgTran pfTypedId arg, :argList]
--          pfNothing? pfTypedType arg =>
--            argTypeList := [nil, :argTypeList]
--          argTypeList := [pf2Atree1 pfTypedType arg, :argTypeList]
--        systemError '"definition args should be typed"
--      argList := reverse! argList
--      retType :=
--        pfNothing? pfLambdaRets pf => nil
--        pf2Atree1 pfLambdaRets pf
--      argTypeList := [retType, :reverse! argTypeList]
--      [argList, :[argTypeList, [nil for arg in argTypeList],
--        pf2Atree1 pfLambdaBody pf]]
--    ['id, :['(()), '(()), pf2Atree1 pf]]
--
--  pfLambda2Atree pf ==
--    [argList, :body] := pfLambdaTran pf
--    ["ADEF", argList, :body]
--
--  pfCollectArgTran pf ==
--    pfCollect? pf =>
--      conds := [pf2Atree1 x for x in pfParts pfCollectIterators pf]
--      id := pf2Atree1 pfCollectBody pf
--      conds is [["|", cond]] =>
--        ["|", id, cond]
--      [id, :conds]
--    pf2Atree1 pf
--

pfSequence2Atree pf ==
    $insideSEQ:             local := true
    ($insideSEQ): local := true

    seq := pfSequence2Atree0([pf2Atree1 x for x in (pf0SequenceArgs)(pf)], pf)
    seqSex := (pfSequence2Sex0)([pf2Sexpr(x) for x in (pf0SequenceArgs)(pf)])
    seqSex is ["SEQ", :ruleList] and ruleList is [["rule", :.], :.] =>
        [mkAtreeNodeWithSrcPos("ruleset",pf),
            [mkAtreeNodeWithSrcPos("construct",pf), :rest seq]]
    seq

pfSequence2Atree0(seqList, pf) ==
    null seqList => "%noBranch"
    seqTranList := []
    while seqList ~= nil repeat
        item := first seqList
        item is [exitVal, cond, value] and getUnname(exitVal) = "exit" =>
            item := [mkAtreeNodeWithSrcPos("IF",pf), cond, value,
                pfSequence2Atree0(rest seqList, pf)]
            seqTranList := [item, :seqTranList]
            seqList := nil
        seqTranList := [item ,:seqTranList]
        seqList := rest seqList
    #seqTranList = 1 => first seqTranList
    [mkAtreeNodeWithSrcPos("SEQ",pf), :reverse! seqTranList]

--
--  float2Atree num ==
--    eIndex := findChar(char "e", num)
--    mantPart :=
--      eIndex => subSequence(num, 0, eIndex)
--      num
--    expPart := (eIndex => readLispFromString subSequence(num, eIndex+1); 0)
--    dotIndex := findChar(char ".", mantPart)
--    intPart :=
--      dotIndex => readLispFromString subSequence(mantPart, 0, dotIndex)
--      readLispFromString mantPart
--    fracPartString :=
--      dotIndex => subSequence(mantPart, dotIndex+1)
--      '"0"
--    bfForm := MAKE_-FLOAT(intPart, readLispFromString fracPartString,
--      # fracPartString, expPart)
--    [., frac, :exp] := bfForm
--    [["$elt", intNewFloat(), 'float], frac, exp, 10]
--

loopIters2Atree iterList ==
    -- could probably do a better job of getting accurate SrcPos info onto parts
    result := nil
    for iter in iterList repeat
        -- ON and UNTIL forms are no longer supported
        sex := pf2Sexpr(iter)
        sex is ['IN, var, ['SEGMENT, i, ["BY", incr]]] =>
            newIter := ["STEP", var, mkAtree1WithSrcPos(i,iter),
                mkAtree1WithSrcPos(incr, iter)]
            result := [newIter, :result]
        sex is ['IN, var, ["BY", ['SEGMENT, i, j], incr]] =>
            newIter := ["STEP", var, mkAtree1WithSrcPos(i,iter),
                mkAtree1WithSrcPos(incr, iter), mkAtree1WithSrcPos(j,iter)]
            result := [newIter, :result]
        sex is ['IN, var, ['SEGMENT, i, j]] =>
            newIter := ["STEP", var, mkAtree1WithSrcPos(i,iter),
                mkAtree1WithSrcPos(1,iter), mkAtree1WithSrcPos(j,iter)]
            result := [newIter, :result]
        sex is ['IN, var, s] =>
            newIter := ["IN", var, mkAtree1 s]
            result := [newIter, :result]
        result := [pf2Atree1(iter), :result]
    reverse! result

pfCollect2Atree pf ==
    atree := [mkAtree1WithSrcPos("COLLECT",pf),
        :loopIters2Atree (pfParts)((pfCollectIterators)(pf)),
            pf2Atree1 (pfCollectBody)(pf)]

    -- next are for what appears to a parser screw-up
    sex := ["COLLECT",
        :(loopIters2Sex)((pfParts)((pfCollectIterators)(pf))),
            pf2Sexpr (pfCollectBody)(pf)]
    sex is ["COLLECT", ["|", cond], var] and symbol? var =>
        [., [., condAtree], varAtree] := atree
        ["SUCHTHAT", varAtree, condAtree]

    atree

--
--  pfRule2Atree pf ==
--    $quotedOpList:local := nil
--    $predicateList:local := nil
--    $multiVarPredicateList:local := nil
--    lhs := pfLhsRule2Atree pfRuleLhsItems pf
--    rhs := pfRhsRule2Atree pfRuleRhs pf
--    lhs := ruleLhsTran lhs
--    rulePredicateTran
--      $quotedOpList => ["rule", lhs, rhs, ["construct", :$quotedOpList]]
--      ["rule", lhs, rhs]
--
--
--  ruleLhsTran ruleLhs ==
--    for pred in $predicateList repeat
--      [name, predLhs, :predRhs] := pred
--      vars := patternVarsOf predRhs
--      rest vars =>  -- if there is more than one patternVariable
--        ruleLhs := substitute!(predLhs, name, ruleLhs)
--        $multiVarPredicateList := [pred, :$multiVarPredicateList]
--      predicate :=
--        [., var] := predLhs
--        ["suchThat", predLhs, ["ADEF", [var],
--          '((Boolean) (Expression (Integer))), '(() ()), predRhs]]
--      ruleLhs := substitute!(predicate, name, ruleLhs)
--    ruleLhs
--
--  rulePredicateTran rule ==
--    null $multiVarPredicateList => rule
--    varList := patternVarsOf [rhs for [.,.,:rhs] in $multiVarPredicateList]
--    predBody :=
--      rest $multiVarPredicateList =>
--        ['AND, :[:pvarPredTran(rhs, varList) for [.,.,:rhs] in
--          $multiVarPredicateList]]
--      [[.,.,:rhs],:.] := $multiVarPredicateList
--      pvarPredTran(rhs, varList)
--    ['suchThat, rule,
--     ['construct, :[quote var for var in varList]],
--      ['ADEF, '(predicateVariable),
--       '((Boolean) (List (Expression (Integer)))), '(() ()),
--        predBody]]
--
--  pvarPredTran(rhs, varList) ==
--    for var in varList for i in 1.. repeat
--      rhs := substitute!(['elt, 'predicateVariable, i], var, rhs)
--    rhs
--
--  patternVarsOf expr ==
--    patternVarsOf1(expr, nil)
--
--  patternVarsOf1(expr, varList) ==
--    null expr => varList
--    expr isnt [.,:.] =>
--      not symbol? expr => varList
--      SymMemQ(expr, varList) => varList
--      [expr, :varList]
--    expr is [op, :argl] =>
--      for arg in argl repeat
--        varList := patternVarsOf1(arg, varList)
--      varList
--    varList
--
--  pfLhsRule2Atree lhs ==
--    $insideRule: local := 'left
--    ($insideRule): local := 'left
--    pf2Atree1 lhs
--
--
--  pfRhsRule2Atree rhs ==
--    $insideRule: local := 'right
--    ($insideRule): local := 'right
--    pf2Atree1 rhs
--

--  pfSuchThat2Atree args ==
--    name := gensym()
--    argList := pf0TupleParts args
--    lhsSex := pf2Atree1 first argList
--    rhsSex := pf2Atree second argList
--    $predicateList := [[name, lhsSex, :rhsSex], :$predicateList]
--    name

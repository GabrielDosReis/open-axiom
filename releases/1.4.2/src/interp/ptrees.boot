-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
-- Copyright (C) 2007-2011, Gabriel Dos Reis.
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



-- This file provides functions to create and examine abstract
-- syntax trees.  These are called pform, for short.
-- The definition of valid pforms see ABSTRACT BOOT.

-- !! This file also contains constructors for concrete syntax, although
-- !! they should be somewhere else.

-- THE PFORM DATA STRUCTURE
--  Leaves: [hd, tok, pos]
--  Trees:  [hd, tree, tree, ...]
--  hd is either an id or (id . alist)



import posit
import serror
namespace BOOT
module ptrees

--% This structure is incomplet
structure %Ast ==
  %Exist(%Vars,%Ast)
  %Forall(%Vars,%Ast)

--% SPECIAL NODES
pfListOf x          == pfTree('listOf,x)
pfListOf? x         == pfAbSynOp?(x,'listOf)
pfAppend ls ==
  ls isnt [l,:ls] => nil
  r := copyList l
  p := r
  repeat
    ls isnt [l,:ls] => return r
    l = nil => nil
    lastNode(p).rest := copyList l
    p := rest p

pfNothing ()        == pfTree('nothing, [])
pfNothing? form     == pfAbSynOp?(form, 'nothing)

-- SemiColon

pfSemiColon(pfbody) == pfTree('SemiColon, [pfbody])
pfSemiColon?(pf)    == pfAbSynOp? (pf, 'SemiColon)
pfSemiColonBody pf   == second pf       -- was ==>

--% Renaming.
--% We decided that the identifier "^" is syntactically synonymous to "**".
--% We could have banned the later and support only the former.
--% However, that would have made all exponentiation examples from
--% the Jenks&Sutor book invalid.  Which would be infortunate.
--% Rather, we opt to the renaming here.  This poses the danger that
--% any other AST scheme would need to do the same work.
--%     --gdr, 2009-10-08.
pfRename x ==
  x = "^" => "**"
  x

--% LEAVES
pfId(expr)               == pfLeaf('id, expr)
pfIdPos(expr,pos)        == pfLeaf('id,expr,pos)
pfId? form               ==
        pfAbSynOp?(form,'id) or pfAbSynOp?(form,'idsy)
pfSymbolVariable? form   == pfAbSynOp?(form,'idsy)
pfIdSymbol form          == pfRename tokPart form
--pfAmpersand(amptok,name) == name

pfDocument strings       == pfLeaf('Document, strings)
pfDocument? form         == pfAbSynOp?(form, 'Document)
pfDocumentText form      == tokPart form

pfDefinableName? form ==
  pfAbSynOp form in '(id integer)

pfLiteral? form ==
  pfAbSynOp form in '(integer symbol expression one zero char string float)

pfLiteralClass form      == pfAbSynOp form
pfLiteralString form     == tokPart form

pfStringConstString form     == tokPart form

pfExpression(expr, :optpos) ==
               pfLeaf("expression", expr, IFCAR optpos)
pfExpression? form          == pfAbSynOp?(form, 'expression)

pfSymbol(expr, :optpos) ==
               pfLeaf("symbol", expr, IFCAR optpos)

pfSymb(expr, :optpos) ==
         if pfLeaf? expr
         then pfSymbol(tokPart expr,IFCAR optpos)
         else pfExpression(pfSexpr expr,IFCAR optpos)

pfSymbol? form          == pfAbSynOp?(form, 'symbol)


pfSymbolSymbol form     == pfRename tokPart form

--% TREES
-- parser interface functions
-- these are potential sources of trouble in macro expansion

-- the comment is attached to all signatutres
pfWDec(doc,name)   == [pfWDeclare(i,doc) for i in pfParts name]

pfTweakIf form==
    a:=pfIfElse form
    b:=if pfNothing? a then pfListOf [] else a
    pfTree('WIf,[pfIfCond form,pfIfThen form,b])

pfInfApplication(op,left,right)==
   pfCheckInfop left =>
       pfWrong(pfDocument ['"infop as argument to infop"],pfListOf [])
   pfCheckInfop right =>
       pfWrong(pfDocument ['"infop as argument to infop"],pfListOf [])
   pfIdSymbol op = "and" => pfAnd (left,right)
   pfIdSymbol op = "or" => pfOr (left,right)
   pfApplication(pfRename op,pfTuple pfListOf [left,right])

pfCheckInfop form== false

pfAnd(pfleft, pfright) == pfTree('And, [pfleft, pfright])
pfAnd?(pf) == pfAbSynOp? (pf, 'And)
pfAndLeft pf == second pf       -- was ==>
pfAndRight pf == third pf       -- was ==>

pfOr(pfleft, pfright) == pfTree('Or, [pfleft, pfright])
pfOr?(pf) == pfAbSynOp? (pf, 'Or)
pfOrLeft pf == second pf       -- was ==>
pfOrRight pf == third pf       -- was ==>

pfNot(arg) == pfTree('Not, [arg])
pfNot?(pf) == pfAbSynOp? (pf, 'Not)
pfNotArg pf == second pf       -- was ==>

pfEnSequence a==
           if null a
           then  pfTuple pfListOf a
           else if null rest a
                then  first a
                else  pfSequence pfListOf a
pfFromDom(dom,expr)==
    if pfApplication? expr
    then pfApplication(pfFromdom(pfApplicationOp expr,dom),
                       pfApplicationArg expr)
    else pfFromdom(expr,dom)

pfReturnTyped(type,body)==pfTree('returntyped,[type,body])

pfLam(variable,body)==-- called from parser
    rets:= if pfAbSynOp?(body,'returntyped)
           then pfFirst body
           else pfNothing ()
    bdy:= if pfAbSynOp?(body,'returntyped) then pfSecond body else body
    pfLambda(variable,rets,bdy)

pfTLam(variable,body)==-- called from parser
    rets:= if pfAbSynOp?(body,'returntyped)
           then pfFirst body
           else pfNothing ()
    bdy:= if pfAbSynOp?(body,'returntyped) then pfSecond body else body
    pfTLambda(variable,rets,bdy)

pfIfThenOnly(pred,first)==pfIf(pred,first,pfNothing())

pfLp(iterators,body)==
       pfLoop pfListOf [:iterators,pfDo body]
pfLoop1 body == pfLoop pfListOf [pfDo body]


pfExitNoCond value== pfExit(pfNothing(),value)

pfReturnNoName(value)==pfReturn(value,pfNothing())

pfBrace(a,part)==pfApplication(pfIdPos( "{}",tokPosn a),part)

pfBracket(a,part) ==  pfApplication(pfIdPos( "[]",tokPosn a),part)
pfBraceBar(a,part)==pfApplication(pfIdPos( "{||}",tokPosn a),part)

pfBracketBar(a,part) ==  pfApplication(pfIdPos( "[||]",tokPosn a),part)
pfHide(a,part) ==   pfTree("Hide",[part])
pfHide? x== pfAbSynOp?(x,"Hide")
pfHidePart x== second x
pfParen(a,part)==part

pfPile(part)==part

pfSpread(l,t)==  [pfTyped(i,t) for i in l]

pfTupleList form== pfParts pfTupleParts form

--The rest have been generated from ABCUT INPUT
-- 1/31/89


--              Add         / Application  / Assign /
--              Coerceto    / Collect      / ComDefinition  / DeclPart /
--              Exit        / Export       / Free /
--              Fromdom     / Id           / If            / Inline /
--              Iterate     / Lambda /
--              Break       / Literal      / Local         / Loop   /
--              MLambda     / Pretend      / Restrict    / Return /
--              Sequence    / Tagged       / Tuple         / Typing /
--              Where       / With

pfExpr? pf ==
     pfAdd? pf or _
     pfApplication? pf or _
     pfAssign? pf or _
     pfCoerceto? pf or _
     pfCollect? pf or _
     pfComDefinition? pf or _
     pfDeclPart? pf or _
     pfExit? pf or _
     pfExport? pf or _
     pfFree? pf or _
     pfFromdom? pf or _
     pfId? pf or _
     pfIf? pf or _
     pfInline? pf or _
     pfIterate? pf or _
     pfLambda? pf or _
     pfBreak? pf or _
     pfLiteral? pf or _
     pfLocal? pf or _
     pfLoop? pf or _
     pfMLambda? pf or _
     pfPretend? pf or _
     pfRestrict? pf or _
     pfReturn? pf or _
     pfTagged? pf or _
     pfTuple? pf or _
     pfWhere? pf or _
     pfWith? pf


pfDeclPart? pf ==
     pfTyping? pf or _
     pfImport? pf or _
     pfDefinition? pf or _
     pfSequence? pf or _
     pfDWhere? pf or _
     pfMacro? pf


-- Wrong       := (Why: Document, Rubble: [Expr])

pfWrong(pfwhy, pfrubble) == pfTree('Wrong, [pfwhy, pfrubble])
pfWrong?(pf) == pfAbSynOp? (pf, 'Wrong)
pfWrongWhy pf == second pf       -- was ==>
pfWrongRubble pf == third pf       -- was ==>
pf0WrongRubble pf == pfParts pfWrongRubble pf


-- Add         := (Base: [Typed],  Addin: Expr)

pfAdd(pfbase, pfaddin,:addon) ==
    lhs := if addon
           then first addon
           else pfNothing()
    pfTree('Add, [pfbase, pfaddin,lhs])

pfAdd?(pf) == pfAbSynOp? (pf, 'Add)
pfAddBase pf == second pf       -- was ==>
pfAddAddin pf == third pf       -- was ==>
pfAddAddon pf == fourth pf       -- was ==>
pf0AddBase pf == pfParts pfAddBase pf



-- DWhere      := (Context: [DeclPart], Expr: [DeclPart])

pfDWhere(pfcontext, pfexpr) == pfTree('DWhere, [pfcontext, pfexpr])
pfDWhere?(pf) == pfAbSynOp? (pf, 'DWhere)
pfDWhereContext pf == second pf       -- was ==>
pfDWhereExpr pf == third pf       -- was ==>



-- With        := (Base: [Typed],  Within: [WithPart])

pfWith(pfbase, pfwithin) ==
         pfTree('With, [pfbase,pfwithin])
pfWith?(pf) == pfAbSynOp? (pf, 'With)
pfWithBase pf == second pf 
pfWithWithin pf == third pf
pf0WithBase pf == pfParts pfWithBase pf
pf0WithWithin pf == pfParts pfWithWithin pf


-- WIf         := (Cond: Primary, Then: [WithPart], Else: [WithPart])

pfWIf(pfcond, pfthen, pfelse) == pfTree('WIf, [pfcond, pfthen, pfelse])
pfWIf?(pf) == pfAbSynOp? (pf, 'WIf)
pfWIfCond pf == second pf       -- was ==>
pfWIfThen pf == third pf       -- was ==>
pfWIfElse pf == fourth pf       -- was ==>

-- WDeclare    := (Signature: Typed, Doc: ? Document)

pfWDeclare(pfsignature, pfdoc) == pfTree('WDeclare, [pfsignature, _
pfdoc])
pfWDeclare?(pf) == pfAbSynOp? (pf, 'WDeclare)
pfWDeclareSignature pf == second pf       -- was ==>
pfWDeclareDoc pf == third pf       -- was ==>


-- Attribute   := (Expr: Primary)

pfAttribute(pfexpr) == pfTree('%Attribute, [pfexpr])
pfAttribute?(pf) == pfAbSynOp? (pf, '%Attribute)
pfAttributeExpr pf == second pf       -- was ==>


-- Typed       := (Id: Id,    Type: ? Type)

pfTyped(pfid, pftype) == pfTree('Typed, [pfid, pftype])
pfTyped?(pf) == pfAbSynOp? (pf, 'Typed)
pfTypedId pf == second pf       -- was ==>
pfTypedType pf == third pf       -- was ==>


-- Application := (Op:   Expr, Arg:    Expr)

pfApplication(pfop, pfarg) ==
        pfTree('Application, [pfop, pfarg])

pfApplication?(pf) == pfAbSynOp? (pf, 'Application)
pfApplicationOp pf == second pf       -- was ==>
pfApplicationArg pf == third pf       -- was ==>


-- Tuple       := (Parts: [Expr])

pfTupleListOf(pfparts) == pfTuple pfListOf pfparts
pfTuple(pfparts) == pfTree("tuple", [pfparts])
pfTuple?(pf) == pfAbSynOp? (pf, "tuple")
pfTupleParts pf == second pf       -- was ==>
pf0TupleParts pf == pfParts pfTupleParts pf


-- Tagged      := (Tag:  Expr, Expr:   Expr)

pfTagged(pftag, pfexpr) == pfTree('Tagged, [pftag, pfexpr])
pfTagged?(pf) == pfAbSynOp? (pf, 'Tagged)
pfTaggedTag pf == second pf       -- was ==>
pfTaggedExpr pf == third pf       -- was ==>


-- Pretend     := (Expr: Expr, Type:   Type)

pfPretend(pfexpr, pftype) == pfTree('Pretend, [pfexpr, pftype])
pfPretend?(pf) == pfAbSynOp? (pf, 'Pretend)
pfPretendExpr pf == second pf       -- was ==>
pfPretendType pf == third pf       -- was ==>


-- Restrict    := (Expr: Expr, Type:   Type)

pfRestrict(pfexpr, pftype) == pfTree('Restrict, [pfexpr, pftype])
pfRestrict?(pf) == pfAbSynOp? (pf, 'Restrict)
pfRestrictExpr pf == second pf       -- was ==>
pfRestrictType pf == third pf       -- was ==>

pfRetractTo(pfexpr, pftype) == pfTree('RetractTo, [pfexpr, pftype])
pfRetractTo?(pf) == pfAbSynOp? (pf, 'RetractTo)
pfRetractToExpr pf == second pf       -- was ==>
pfRetractToType pf == third pf       -- was ==>


-- Coerceto    := (Expr: Expr, Type:   Type)

pfCoerceto(pfexpr, pftype) == pfTree('Coerceto, [pfexpr, pftype])
pfCoerceto?(pf) == pfAbSynOp? (pf, 'Coerceto)
pfCoercetoExpr pf == second pf       -- was ==>
pfCoercetoType pf == third pf       -- was ==>


-- Fromdom     := (What: Id,   Domain: Type)

pfFromdom(pfwhat, pfdomain) == pfTree('Fromdom, [pfwhat, pfdomain])
pfFromdom?(pf) == pfAbSynOp? (pf, 'Fromdom)
pfFromdomWhat pf == second pf       -- was ==>
pfFromdomDomain pf == third pf       -- was ==>


-- Lambda     := (Args: [Typed], Rets: ? Type, Body: Expr)

pfLambda(pfargs, pfrets, pfbody) == pfTree('Lambda, [pfargs, pfrets, _
pfbody])
pfLambda?(pf) == pfAbSynOp? (pf, 'Lambda)
pfLambdaArgs pf == second pf       -- was ==>
pfLambdaRets pf == third pf       -- was ==>
pfLambdaBody pf == fourth pf       -- was ==>
pf0LambdaArgs pf == pfParts pfLambdaArgs pf
pfFix pf== pfApplication(pfId "Y",pf)


-- TLambda      := (Args: [Typed], Rets: ? Type, Body: Expr)

pfTLambda(pfargs, pfrets, pfbody) == pfTree('TLambda, [pfargs, pfrets, pfbody])
pfTLambda?(pf) == pfAbSynOp? (pf, 'TLambda)
pfTLambdaArgs pf == second pf       -- was ==>
pfTLambdaRets pf == third pf       -- was ==>
pfTLambdaBody pf == fourth pf       -- was ==>
pf0TLambdaArgs pf == pfParts pfTLambdaArgs pf


-- MLambda     := (Args: [Id], Body: Expr)

pfMLambda(pfargs, pfbody) == pfTree('MLambda, [pfargs, pfbody])
pfMLambda?(pf) == pfAbSynOp? (pf, 'MLambda)
pfMLambdaArgs pf == second pf       -- was ==>
pfMLambdaBody pf == third pf       -- was ==>
pf0MLambdaArgs pf == pfParts pfMLambdaArgs pf


-- Where       := (Context: [DeclPart], Expr: Expr)

pfWhere(pfcontext, pfexpr) == pfTree('Where, [pfcontext, pfexpr])
pfWhere?(pf) == pfAbSynOp? (pf, 'Where)
pfWhereContext pf == second pf       -- was ==>
pfWhereExpr pf == third pf       -- was ==>
pf0WhereContext pf == pfParts pfWhereContext pf


-- If          := (Cond: Expr, Then: Expr, Else: ? Expr)

pfIf(pfcond, pfthen, pfelse) == pfTree('If, [pfcond, pfthen, pfelse])
pfIf?(pf) == pfAbSynOp? (pf, 'If)
pfIfCond pf == second pf       -- was ==>
pfIfThen pf == third pf       -- was ==>
pfIfElse pf == fourth pf       -- was ==>

-- %Match := (Expr: Expr, Alts: [Exit])

pfCase(pfexpr, pfalts) == pfTree("%Match",[pfexpr,pfalts])
pfCase? pf == pfAbSynOp?(pf,"%Match")
pfCaseScrutinee pf == second pf
pfCaseAlternatives pf == third pf

-- Sequence    := (Args: [Expr])

pfSequence(pfargs) == pfTree('%Sequence, [pfargs])
pfSequence?(pf) == pfAbSynOp? (pf, '%Sequence)
pfSequenceArgs pf == second pf       -- was ==>
pf0SequenceArgs pf == pfParts pfSequenceArgs pf


-- Novalue     := (Expr: Expr)

pfNovalue(pfexpr) == pfTree('Novalue, [pfexpr])
pfNovalue?(pf) == pfAbSynOp? (pf, 'Novalue)
pfNovalueExpr pf == second pf       -- was ==>


-- Loop        := (Iterators: [Iterator])

pfLoop(pfiterators) == pfTree('Loop, [pfiterators])
pfLoop?(pf) == pfAbSynOp? (pf, 'Loop)
pfLoopIterators pf == second pf       -- was ==>
pf0LoopIterators pf == pfParts pfLoopIterators pf


-- Collect     := (Body: Expr, Iterators: [Iterator])

pfCollect(pfbody, pfiterators) == pfTree('Collect, [pfbody, _
pfiterators])
pfCollect?(pf) == pfAbSynOp? (pf, 'Collect)
pfCollectBody pf == second pf       -- was ==>
pfCollectIterators pf == third pf       -- was ==>
pf0CollectIterators pf == pfParts pfCollectIterators pf


-- Forin       := (Lhs: [AssLhs], Whole: Expr)

pfForin(pflhs, pfwhole) == pfTree('Forin, [pflhs, pfwhole])
pfForin?(pf) == pfAbSynOp? (pf, 'Forin)
pfForinLhs pf == second pf       -- was ==>
pfForinWhole pf == third pf       -- was ==>
pf0ForinLhs pf == pfParts pfForinLhs pf


-- While       := (Cond: Expr)

pfWhile(pfcond) == pfTree('While, [pfcond])
pfWhile?(pf) == pfAbSynOp? (pf, 'While)
pfWhileCond pf == second pf       -- was ==>


-- Until       := (Cond: Expr)

--pfUntil(pfcond) == pfTree('Until, [pfcond])
--pfUntil?(pf) == pfAbSynOp? (pf, 'Until)
--pfUntilCond pf == second pf       -- was ==>


-- Suchthat    := (Cond: Expr)

pfSuchthat(pfcond) == pfTree('Suchthat, [pfcond])
pfSuchthat?(pf) == pfAbSynOp? (pf, 'Suchthat)
pfSuchthatCond pf == second pf       -- was ==>


-- Do          := (Body: Expr)

pfDo(pfbody) == pfTree('Do, [pfbody])
pfDo?(pf) == pfAbSynOp? (pf, 'Do)
pfDoBody pf == second pf       -- was ==>


-- Iterate     := (From: ? Id)

pfIterate(pffrom) == pfTree('Iterate, [pffrom])
pfIterate?(pf) == pfAbSynOp? (pf, 'Iterate)
pfIterateFrom pf == second pf       -- was ==>


-- Break       := (From: ? Id)

pfBreak(pffrom) == pfTree('Break, [pffrom])
pfBreak?(pf) == pfAbSynOp? (pf, 'Break)
pfBreakFrom pf == second pf       -- was ==>


-- Return      := (Expr: ? Expr, From: ? Id)

pfReturn(pfexpr, pffrom) == pfTree('Return, [pfexpr, pffrom])
pfReturn?(pf) == pfAbSynOp? (pf, 'Return)
pfReturnExpr pf == second pf       -- was ==>
pfReturnFrom pf == third pf       -- was ==>


-- Exit        := (Cond: ? Expr, Expr: ? Expr)

pfExit(pfcond, pfexpr) == pfTree('Exit, [pfcond, pfexpr])
pfExit?(pf) == pfAbSynOp? (pf, 'Exit)
pfExitCond pf == second pf       -- was ==>
pfExitExpr pf == third pf       -- was ==>


-- Macro       := (Lhs:  Id,     Rhs: ExprorNot)

pfMacro(pflhs, pfrhs) == pfTree('Macro, [pflhs, pfrhs])
pfMacro?(pf) == pfAbSynOp? (pf, 'Macro)
pfMacroLhs pf == second pf       -- was ==>
pfMacroRhs pf == third pf       -- was ==>


-- Definition  := (LhsItems:  [Typed], Rhs:  Expr)

pfDefinition(pflhsitems, pfrhs) == pfTree('Definition, [pflhsitems, pfrhs])
pfDefinition?(pf) == pfAbSynOp? (pf, 'Definition)
pfDefinitionLhsItems pf == second pf       -- was ==>
pfDefinitionRhs pf == third pf       -- was ==>
pf0DefinitionLhsItems pf == pfParts pfDefinitionLhsItems pf

pfRule(pflhsitems, pfrhs) == pfTree('Rule, [pflhsitems, _
pfrhs])
pfRule?(pf) == pfAbSynOp? (pf, 'Rule)
pfRuleLhsItems pf == second pf       -- was ==>
pfRuleRhs pf == third pf       -- was ==>

-- ComDefinition := (Doc:Document,Def:Definition)

pfComDefinition(pfdoc, pfdef) == pfTree('ComDefinition, [pfdoc, pfdef] )
pfComDefinition?(pf) == pfAbSynOp? (pf, 'ComDefinition)
pfComDefinitionDoc pf == second pf       -- was ==>
pfComDefinitionDef pf == third pf       -- was ==>


-- DefinitionSequence    := (Args: [DeclPart])

pfDefinitionSequenceArgs pf == second pf       -- was ==>

-- Export      := (Def:   Definition)

pfExportDef pf == second pf       -- was ==>

-- Assign      := (LhsItems:  [AssLhs], Rhs:  Expr)

pfAssign(pflhsitems, pfrhs) == pfTree('Assign, [pflhsitems, pfrhs])
pfAssign?(pf) == pfAbSynOp? (pf, 'Assign)
pfAssignLhsItems pf == second pf       -- was ==>
pfAssignRhs pf == third pf       -- was ==>
pf0AssignLhsItems pf == pfParts pfAssignLhsItems pf


-- Typing      := (Items: [Typed])

pfTyping(pfitems) == pfTree('Typing, [pfitems])
pfTyping?(pf) == pfAbSynOp? (pf, 'Typing)
pfTypingItems pf == second pf       -- was ==>
pf0TypingItems pf == pfParts pfTypingItems pf


-- Export       := (Items: [Typed])

pfExport(pfitems) == pfTree('Export, [pfitems])
pfExport?(pf) == pfAbSynOp? (pf, 'Export)
pfExportItems pf == second pf       -- was ==>
pf0ExportItems pf == pfParts pfExportItems pf


-- Local       := (Items: [Typed])

pfLocal(pfitems) == pfTree('Local, [pfitems])
pfLocal?(pf) == pfAbSynOp? (pf, 'Local)
pfLocalItems pf == second pf       -- was ==>
pf0LocalItems pf == pfParts pfLocalItems pf

-- Free        := (Items: [Typed])

pfFree(pfitems) == pfTree('Free, [pfitems])
pfFree?(pf) == pfAbSynOp? (pf, 'Free)
pfFreeItems pf == second pf       -- was ==>
pf0FreeItems pf == pfParts pfFreeItems pf


-- Import      := (Items: [QualType])

pfImport(pfitems) == pfTree('Import, [pfitems])
pfImport?(pf) == pfAbSynOp? (pf, 'Import)
pfImportItems pf == second pf       -- was ==>
pf0ImportItems pf == pfParts pfImportItems pf


-- Inline      := (Items: [QualType])

pfInline(pfitems) == pfTree('Inline, [pfitems])
pfInline?(pf) == pfAbSynOp? (pf, 'Inline)
pfInlineItems pf == second pf       -- was ==>
pf0InlineItems pf == pfParts pfInlineItems pf

-- QualType    := (Type: Type, Qual: ? Type)

pfQualType(pftype, pfqual) == pfTree('QualType, [pftype, pfqual])
pfQualType?(pf) == pfAbSynOp? (pf, 'QualType)
pfQualTypeType pf == second pf       -- was ==>
pfQualTypeQual pf == third pf       -- was ==>

pfSuch(x,y)== pfInfApplication(pfId "|",x,y)

pfTaggedToTyped x==
  rt:=if pfTagged? x then pfTaggedExpr x else pfNothing()
  form:= if pfTagged? x then pfTaggedTag x else x
  not pfId? form =>
      a:=pfId gensym()
      pfTyped(pfSuch(a,
           pfInfApplication (pfId "=", a,form)),rt)
  pfTyped(form,rt)

pfTaggedToTyped1 x==
    pfCollect1? x => pfCollectVariable1 x
    pfDefinition? x => pfTyped(x,pfNothing())
    pfTaggedToTyped x

pfCollectVariable1 x==
      a := pfApplicationArg x
      var:=first pf0TupleParts a
      id:=pfTaggedToTyped var
      pfTyped(pfSuch(pfTypedId id,second pf0TupleParts a),
              pfTypedType id)

pfPushBody(t,args,body)==
        if null args
        then  body
        else if null rest args
              then  pfLambda(first args,t,body)
              else
                 pfLambda(first args,pfNothing(),
                     pfPushBody(t,rest args,body))

pfCheckItOut x ==
  rt:=if pfTagged? x then pfTaggedExpr x else pfNothing()
  form:= if pfTagged? x then pfTaggedTag x else x
  pfDefinableName? form => [pfListOf [pfTyped(form,rt)],nil,rt]
  pfCollect1? form =>
                [pfListOf [pfCollectVariable1 form],nil,rt]
  pfTuple? form =>
       [pfListOf [pfTaggedToTyped i for i in pf0TupleParts form],nil,rt]
  pfDefinition? form =>
       [pfListOf [pfTyped(form,pfNothing())],nil,rt]
  pfApplication? form =>
          ls:=pfFlattenApp form
          op:= pfTaggedToTyped1 first ls
          args:=[pfTransformArg i for i in rest ls]
          [pfListOf [op],args,rt]
  npTrapForm form

pfCollect1? x==
        pfApplication? x =>
              a:=pfApplicationOp x
              pfId? a => pfIdSymbol a = "|"
              false
        false

pfTransformArg  args==
          argl:= if pfTuple? args then pf0TupleParts args else [args]
          pfListOf [pfTaggedToTyped1 i for i in argl]


pfCheckMacroOut form ==
  pfId? form => [form,nil]
  pfApplication? form =>
          ls:=pfFlattenApp form
          op:= pfCheckId first ls
          args:=[pfCheckArg i for i in rest ls]
          [op,args]
  npTrapForm form

pfCheckArg args==
          argl:= if pfTuple? args then pf0TupleParts args else [args]
          pfListOf [pfCheckId i for i in argl]

pfCheckId form==   if not pfId? form then npTrapForm(form) else form

pfPushMacroBody(args,body)==
    null args =>   body
    pfMLambda(first args,pfPushMacroBody(rest args,body))

pfFlattenApp x==
   pfApplication? x=>
             pfCollect1? x =>[ x ]
             append (pfFlattenApp pfApplicationOp x,
                        pfFlattenApp pfApplicationArg x)
   [x]


--% Utility operations on Abstract Syntax Trees
 
-- An S-expression which people can read.
pfSexpr pform ==
    strip pform where
         strip pform ==
            pfId? pform       => pfIdSymbol pform
            pfLiteral?  pform => pfLiteralString pform
            pfLeaf? pform     => tokPart pform
 
            pfApplication? pform =>
                args :=
                    a := pfApplicationArg pform
                    if pfTuple? a then pf0TupleParts a else [a]
                [strip p for p in [pfApplicationOp pform, :args]]
 
            [pfAbSynOp pform, :[strip p for p in pfParts pform]]
 
pfCopyWithPos( pform , pos ) == 
    pfLeaf? pform =>         pfLeaf( pfAbSynOp pform , tokPart pform , pos )
    pfTree( pfAbSynOp pform , [ pfCopyWithPos( p , pos ) for p in pfParts pform ] )
 
pfMapParts(f, pform) ==
    pfLeaf? pform => pform
    parts0 := pfParts pform
    parts1 := [FUNCALL(f, p) for p in parts0]
    -- Return the original if no changes.
    same := true
    for p0 in parts0 for p1 in parts1 while same repeat same := sameObject?(p0,p1)
    same => pform
    pfTree(pfAbSynOp pform, parts1)
 
 
pf0ApplicationArgs pform ==
    arg := pfApplicationArg pform
    pf0FlattenSyntacticTuple arg
 
pf0FlattenSyntacticTuple pform ==
    not pfTuple? pform => [pform]
    [:pf0FlattenSyntacticTuple p for p in pf0TupleParts pform]
 

-- Copyright (C) 2011, Gabriel Dos Reis.
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
--     - Neither the name of OpenAxiom. nor the names of its contributors
--       may be used to endorse or promote products derived from this
--       software without specific prior written permission.
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
--

--%
--% Author: Gabriel Dos Reis
--%

import sys_-utility

namespace BOOT

module lexing

--%
--% Token abstract datatype.
--%   Operational semantics:
--%      structure Token ==
--%         Record(symbol: Identifier, type: TokenType, nonBlank?: Boolean)
--%
--%   type in '(NUMBER IDENTIFIER SPECIAL_-CHAR)
--%   nonBlank? if token is not preceded by a blank.
--%
makeToken(sym == nil, typ == nil, blnk? == true) ==
  [sym,typ,blnk?]

macro copyToken t ==
  copyList t

macro tokenSymbol t ==
  first t

macro tokenType t ==
  second t

macro tokenNonblank? t ==
  third t

++ Last seen token
$priorToken := makeToken()

++ Is there no blank in front of current token?
$nonblank := true

++ First token in input stream
$currentToken := makeToken()

++ Next token in input stream
$nextToken := makeToken()

++ Number of token in the buffer (0, 1, 2)
$validTokens := 0

tokenInstall(sym,typ,tok,nonblank == true) ==
  tokenSymbol(tok) := sym
  tokenType(tok) := typ
  tokenNonblank?(tok) := nonblank
  tok

tryGetToken tok ==
  GET_-BOOT_-TOKEN tok =>
   $validTokens := $validTokens + 1
   tok
  nil

++ Returns the current token or gets a new one if necessary
currentToken() ==
  $validTokens > 0 => $currentToken
  tryGetToken $currentToken

++ Returns the token after the current token, or nil if there is none after
nextToken() ==
  currentToken()
  $validTokens > 1 => $nextToken
  tryGetToken $nextToken

matchToken(tok,typ,sym == false) ==
  tok ~= nil and symbolEq?(tokenType tok,typ) and
    (sym = nil or symbolEq?(sym,tokenSymbol tok)) and tok

++ Return the current token if it has type `typ', and possibly the
++ same spelling as `sym'.
matchCurrentToken(typ,sym == nil) ==
  matchToken(currentToken(),typ,sym)

++ Return the next token if it has type `typ;, and possibly the same
++ spelling as `sym'.
matchNextToken(typ,sym == nil) ==
  matchToken(nextToken(),typ,sym)

++ Makes the next token be the current token.
advanceToken() ==
  $validTokens = 0 => tryGetToken $currentToken
  $validTokens = 1 =>
    $validTokens := $validTokens - 1
    $priorToken := copyToken $currentToken
    tryGetToken $currentToken
  $validTokens = 2 =>
    $priorToken := copyToken $currentToken
    $currentToken := copyToken $nextToken
    $validTokens := $validTokens - 1
  nil

makeSymbolOf tok ==
  tok = nil => nil
  tokenSymbol tok = nil => nil
  char? tokenSymbol tok => makeSymbol charString tokenSymbol tok
  tokenSymbol tok

currentSymbol() ==
  makeSymbolOf currentToken()

--%
--% Stack abstract datatype.
--%  Operational semantics:
--%     structure Stack ==
--%         Record(store: List T, size: Integer, top: T, updated?: Boolean)

++ Construct a new stack
makeStack() ==
  [nil,0,nil,false]

macro stackStore st ==
  first st

macro stackSize st ==
  second st

macro stackTop st ==
  third st

macro stackUpdated? st ==
  fourth st

stackLoad!(l,st) ==
  stackStore(st) := l
  stackSize(st) := #l
  stackTop(st) := first l

stackClear! st ==
  stackStore(st) := nil
  stackSize(st) := 0
  stackTop(st) := nil
  stackUpdated?(st) := false

stackPush!(x,st) ==
  stackStore(st) := [x,:stackStore st]
  stackTop(st) := x
  stackSize(st) := stackSize st + 1
  stackUpdated?(st) := true

stackPop! st ==
  y := first stackStore st
  stackStore(st) := rest stackStore st
  stackSize(st) := stackSize st - 1
  if stackStore st ~= nil then
    stackTop(st) := first stackStore st
  y


--%  
--% Parsing reduction stack
--%
--% Abstractly;
--%   structure Reduction == Record(rule: RuleName, value: ParseTree)
--%
makeReduction(p == nil,v == nil) ==
  [p,v]

macro reductionRule r ==
  first r

macro reductionValue r ==
  second r

++ stack of results of reduced productions
$reduceStack := makeStack()

pushReduction(rn,pt) ==
  stackPush!(makeReduction(rn,pt),$reduceStack)

popReduction() ==
  stackPop! $reduceStack
  
reduceStackClear() ==
  stackClear! $reduceStack

popStack1() ==
  reductionValue popReduction()

popStack2() ==
  r1 := popReduction()
  r2 := popReduction()
  stackPush!(r1,$reduceStack)
  reductionValue r2

popStack3() ==
  r1 := popReduction()
  r2 := popReduction()
  r3 := popReduction()
  stackPush!(r2,$reduceStack)
  stackPush!(r1,$reduceStack)
  reductionValue r3

popStack4() ==
  r1 := popReduction()
  r2 := popReduction()
  r3 := popReduction()
  r4 := popReduction()
  stackPush!(r3,$reduceStack)
  stackPush!(r2,$reduceStack)
  stackPush!(r1,$reduceStack)
  reductionValue r4

nthStack n ==
  reductionValue stackStore($reduceStack).(n - 1)

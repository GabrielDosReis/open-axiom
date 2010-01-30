/*
 Copyright (c) 2007 Arthur C. Ralfs
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are
 met:

     - Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

     - Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

     - The name of Arthur C. Ralfs may not be used to endorse or promote 
       products derived from this software without specific prior written 
       permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


/*
 * The first step is to parse the tokens at the top level in texString.
 * Tokens contained in a { ... } group are handled recursively so at
 * the current level { ... } is one token.
 * Say the tokens are tok1 tok2 tok3 tok4 ... tokn
 * then each token is assigned a rank depending on how many look aheads
 * are required to process it.  Thus tokens with rank 0 can be handled
 * as they are peeled of the front of the token list.  If, say, tok3 has
 * rank 1 then to process it requires processing it at the same time as
 * tok2.  In practice I don't think there are any tokens of rank 2, 3, ...
 * however there are tokens that require including all tokens preceeding
 * them when processing. These tokens will be said to have rank -1.
 */



function tex2mml(texString,style) {
    //alert('tex2mml:'+texString+':');
// regexp's for matching tokens
    var variable = new RegExp(/^([a-zA-Z]+)/);
    var number = new RegExp(/^([0-9]+)/);
    var single = new RegExp(/^([-+*/_^()=])/);
    var backWord = new RegExp(/^\\([a-zA-Z]+)/);
    var backSym = new RegExp(/^\\([^a-zA-Z])/);
    if ( style == 'display' ) {
	var mmlString = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\" display=\"block\" mathsize=\"big\">";
    }
    else {
	var mmlString = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\" display=\"inline\" mathsize=\"big\">";
    }
    stringFormat(texString);
    mmlString = mmlString + "</math>";
    return mmlString;

/*
 * This function is needed for recursing down braces
 */

    function stringFormat(texString) {
	//alert('stringFormat start:' + texString + ':');
	texString = trim(texString);
	var tokArray = parseTex(texString);
	var tmpDisplay = "";
	for (var i = 0; i < tokArray.length; i++) {
	    tmpDisplay = tmpDisplay + tokArray[i][0] + ' ';
	}
	//alert(tmpDisplay);
	tokEx(tokArray);
	//alert('stringFormat end');
	return;
    }

    function tokEx(tokArray) {
	//alert('tokEx begin');
	while ( tokArray.length > 0 ) {
	    // itok to hold index of next token to process
	    var itok = 0;
	    for (var i = 0; i < tokArray.length; i++) {
		// Find the next token to process. For now I assume that tokens have only
		// ranks of 0, 1, or -1.
		if ( tokArray[i][2] >= i ) itok = i;
		if (tokArray[i][2] == -1 ) {
		    itok = i;
		    i = tokArray.length; //stop at the first rank -1 token
		}
	    }
/*
	    // if current token = '^' or '_' need to check that preceeding token
	    // isn't \sum, \int, \prod
	    if ( tokArray[itok][0] == '^' || tokArray[itok][0] == '_' ) {
		if ( tokArray[itok-1][1] == 'backWord') {
		    if ( tokArray[itok-1][0] == 'sum' || tokArray[itok-1][0] == 'int' || tokArray[itok-1][0] == 'prod') itok = itok - 1;
		}
	    }
*/
	    tokArray = processTok(itok,tokArray);
	}	
    }

    function processTok(itok,tokArray) {
	//alert('processTok '+tokArray[itok][0]);
	switch(tokArray[itok][1]) {
	    case 'variable':
		tokArray = varFormat(itok,tokArray);
		break;
	    case 'number':
		tokArray = numFormat(itok,tokArray);
		break;
	    case 'operator':
		tokArray = opFormat(itok,tokArray);
		break;
	    case 'backWord':
		tokArray = backWordFormat(itok,tokArray);
		break;
	    case 'backSym':
		tokArray = backSymFormat(itok,tokArray);
		break;
	    case 'brace':
		tokArray = braceFormat(itok,tokArray);
		break;
	}
	return tokArray;
    }

    function braceFormat(itok,tokArray) {
	//alert('braceFormat ' + tokArray[itok][0]);
	// The current token is delimited by braces which need
	// to be stripped off.
	mmlString = mmlString + "<mrow>";
	stringFormat(tokArray[itok][0].slice(1,tokArray[itok][0].length - 1));
	mmlString = mmlString + "</mrow>";
	return tokArray.slice(1);
    }

    function varFormat(itok,tokArray) {
	//alert('varFormat '+tokArray[itok][0]);
	mmlString = mmlString + "<mi>" + tokArray[itok][0] + "</mi>";
	return tokArray.slice(1);
    }

    function numFormat(itok,tokArray) {
	//alert('numFormat '+tokArray[itok][0]);
	mmlString = mmlString + "<mn>" + tokArray[itok][0] + "</mn>";
	return tokArray.slice(1);
    }

/*
 * To add a single operator it must be added here but also to the single
 * regexp expression and, if the operator requires a look back then
 * the special case must be added to parseTex
 */

    function opFormat(itok,tokArray) {
	//alert('opFormat '+ tokArray[itok][0]);
	var tmp;
	var token = tokArray[itok][0];
	//single operators handled: -+*/_^
	//how to handle !?.|/@"()[]
	switch(tokArray[itok][0]) {
	    case '-':
		mmlString = mmlString + "<mo>-</mo>";
		return tokArray.slice(1);
		break;
	    case '+':
		mmlString = mmlString + "<mo>+</mo>";
		return tokArray.slice(1);
		break;
	    case '*':
		mmlString = mmlString + "<mo>*</mo>";
		return tokArray.slice(1);
		break;
	    case '/': // this has rank 1 so itok = 1 and we use itok = 0 and itok = 2
		mmlString = mmlString + "<mfrac>";
		tokEx(tokArray.slice(0,1));
		tokEx(tokArray.slice(2,3));
		mmlString = mmlString + "</mfrac>";
		return tokArray.slice(3);
		break;
	    case '_': // look ahead to see if a ^ follows, i.e. tok0 _ tok2 ^ tok4
		if (tokArray.length > 4 && tokArray[itok+2][0] == '^') {
		    // look back to see if the base if sum, int, prod to use <munderover> 
		    // instead of <msubsup>
		    if ( (tokArray[itok-1][0] == 'sum' || tokArray[itok-1][0] == 'prod' || tokArray[itok-1][0] == 'int') && tokArray[itok-1][1] == 'backWord') {
			var begin = '<munderover>';
			var end = '</munderover>';
		    }
		    else {
			var begin = '<msubsup>';
			var end = '</msubsup>';
		    }
		    mmlString = mmlString + begin;
		    tokEx(tokArray.slice(0,1));
		    tokEx(tokArray.slice(2,3));
		    tokEx(tokArray.slice(4,5));
		    mmlString = mmlString + end;
		    return tokArray.slice(5);
		}
		else { //  tok0 _ tok2
		    // look back to see if the base if sum, int, prod to use <munder> 
		    // instead of <msub>
		    if ( (tokArray[itok-1][0] == 'sum' || tokArray[itok-1][0] == 'prod' || tokArray[itok-1][0] == 'int') && tokArray[itok-1][1] == 'backWord') {
			var begin = '<munder>';
			var end = '</munder>';
		    }
		    else {
			var begin = '<msub>';
			var end = '</msub>';
		    }
		    mmlString = mmlString + begin;
		    tokEx(tokArray.slice(0,1));
		    tokEx(tokArray.slice(2,3));
		    mmlString = mmlString + end;
		    return tokArray.slice(3);
		}
		break;
	    case '^':
                // look ahead to see if a _ follows, i.e. tok0 ^ tok2 _ tok4
		if (tokArray.length > 4 && tokArray[itok+2][0] == '_') {
		    // look back to see if the base is sum, int, prod to use <munderover> 
		    // instead of <msubsup>
		    if ( (tokArray[itok-1][0] == 'sum' || tokArray[itok-1][0] == 'prod' || tokArray[itok-1][0] == 'int') && tokArray[itok-1][1] == 'backWord') {
			var begin = '<munderover>';
			var end = '</munderover>';
		    }
		    else {
			var begin = '<msubsup>';
			var end = '</msubsup>';
		    }
		    mmlString = mmlString + begin;
		    tokEx(tokArray.slice(0,1));
		    tokEx(tokArray.slice(4,5));
		    tokEx(tokArray.slice(2,3));
		    mmlString = mmlString + end;
		    return tokArray.slice(5);
		}
		else { //  tok0 ^ tok2
		    // look back to see if the base if sum, int, prod to use <mover> 
		    // instead of <msup>
		    if ( (tokArray[itok-1][0] == 'sum' || tokArray[itok-1][0] == 'prod' || tokArray[itok-1][0] == 'int') && tokArray[itok-1][1] == 'backWord') {
			var begin = '<mover>';
			var end = '</mover>';
		    }
		    else {
			var begin = '<msup>';
			var end = '</msup>';
		    }
		    mmlString = mmlString + begin;
		    tokEx(tokArray.slice(0,1));
		    tokEx(tokArray.slice(2,3));
		    mmlString = mmlString + end;
		    return tokArray.slice(3);
		}
		break;
	    case '(':
		mmlString = mmlString + "<mo>(</mo>";
		return tokArray.slice(1);
		break;
	    case ')':
		mmlString = mmlString + "<mo>)</mo>";
		return tokArray.slice(1);
		break;
	    case '=':
		mmlString = mmlString + "<mo>=</mo>";
		return tokArray.slice(1);
		break;
	}
	return;
    }

/*
 * For backslash word operators.  Operators like \cos which consume
 * one token and insert a single mathml element are handled directly
 * in backWordFormat.  Backslash operators that consume more than
 * one token are implemented by separate functions.
 */
    function backWordFormat(itok,tokArray) {
	//alert('backWordFormat '+tokArray[itok][0]);
	var tmp;
	switch(tokArray[itok][0]) {
	    case 'sqrt': // tok0 tok1
		mmlString = mmlString + "<msqrt>";
		tokEx(tokArray.slice(1,2));
		mmlString = mmlString + "</msqrt>";
		return tokArray.slice(2);
		break;
	    case 'root': // \root tok1 \of tok3, check that \of is really there
		mmlString = mmlString + "<mroot>";
		tokEx(tokArray.slice(3,4));
		tokEx(tokArray.slice(1,2));
		mmlString = mmlString + "</mroot>";
		return tokArray.slice(4);
		break;
	    case 'of': // this should be handled in conjunction with \root
		return;
		break;
	    case 'underline': // \underline tok1, this isn't quite right but I can't get the  plain underline
		mmlString = mmlString + "<munder accent=\"true\">";
		tokEx(tokArray.slice(1,2));
		mmlString = mmlString + "<mo stretchy=\"true\">&#x023b4;</mo></munder>";
		return tokArray.slice(2);
		break;
	    case 'overline':
		return;
		break;
	    case 'underbrace': // \underbrace tok1, &UnderBrace; should give &#x0fe38;
		mmlString = mmlString + "<munder accent=\"true\">";
		tokEx(tokArray.slice(1,2));
		mmlString = mmlString + "<mo>&#x0fe38;</mo></munder>";
		return tokArray.slice(2);
		break;
	    case 'over': // tok0 tok1 ... tok(n-1) \over tok(n+1) ... tokN
		mmlString = mmlString + "<mfrac>";
		tokEx(tokArray.slice(0,itok));
		tokEx(tokArray.slice(itok+1));
		mmlString = mmlString + "</mfrac>";
		return tokArray.slice(tokArray.length + 1);
		break;
	    case 'frac': // \frac tok1 tok2
		mmlString = mmlString + "<mfrac>";
		tokEx(tokArray.slice(0,itok));
		tokEx(tokArray.slice(itok+1));
		mmlString = mmlString + "</mfrac>";
		return tokArray.slice(tokArray.length + 1);
		break;
	    case 'matrix': // e.g. \matrix {x11 \\amp x12 \\amp x13 \cr x21 \\amp x22 \\amp x23 \cr x31 \\amp x32 \\amp x33 \cr}
		// last \cr is optional
		mmlString = mmlString + "<mtable>";
		// peel off first and last brace
		var tmpString = tokArray[itok+1][0].slice(1,tokArray[itok+1][0].length-1);
		var tmpArray = tmpString.split('\\cr');
		// discard last element if empty string, corresponding to trailing \cr
		if ( tmpArray[tmpArray.length-1] == "" ) tmpArray = tmpArray.slice(0,tmpArray.length-1);
		for ( var row = 0; row < tmpArray.length; row++ ) {
		    mmlString = mmlString + "<mtr>";
		    var eltArray = tmpArray[row].split('\\amp');
		    for ( var elt = 0; elt < eltArray.length; elt++) {
			mmlString = mmlString + "<mtd>";
			stringFormat(eltArray[elt]);
			mmlString = mmlString + "</mtd>";
		    }
		    mmlString = mmlString + "</mtr>";
		}
		mmlString = mmlString + "</mtable>";
		return tokArray.slice(2);
		break;
	    case 'sum':
		mmlString = mmlString + "<mo>&#x02211;</mo>";
		return tokArray.slice(1);
		break;
	    case 'prod':
		mmlString = mmlString + "<mo>&#x0220f;</mo>";
		return tokArray.slice(1);
		break;
	    case 'int':
		mmlString = mmlString + "<mo>&#x0222b;</mo>";
		return tokArray.slice(1);
		break;
	    case 'prime':
		mmlString = mmlString + "<mo>&#x02032;</mo>";
		return tokArray.slice(1);
		break;
	    case 'aleph':
		mmlString = mmlString + "<mi>&#x02135;</mi>";
		return tokArray.slice(1);
		break;
	    case 'hbar':
		mmlString = mmlString + "<mi>&#x0210f;</mi>";
		return tokArray.slice(1);
		break;
	    case 'imath':
		mmlString = mmlString + "<mi>imath</mi>";
		return tokArray.slice(1);
		break;
	    case 'jmath':
		mmlString = mmlString + "<mi>jmath</mi>";
		return tokArray.slice(1);
		break;
	    case 'ell':
		mmlString = mmlString + "<mi>&#x02113</mi>";
		return tokArray.slice(1);
		break;
	    case 'Re':
		mmlString = mmlString + "<mo>Re</mo>";
		return tokArray.slice(1);
		break;
	    case 'Im':
		mmlString = mmlString + "<mo>Im</mo>";
		return tokArray.slice(1);
		break;
	    case 'partial':
		mmlString = mmlString + "<mo>&#x02202;</mo>";
		return tokArray.slice(1);
		break;
	    case 'infty':
		mmlString = mmlString + "<mi>&#x0221e;</mi>";
		return tokArray.slice(1);
		break;
	    case 'emptyset':
		mmlString = mmlString + "<mi>&#x02205;</mi>";
		return tokArray.slice(1);
		break;
	    case 'nabla':
		mmlString = mmlString + "<mo>&#x02207;</mo>";
		return tokArray.slice(1);
		break;
	    case 'surd':
		mmlString = mmlString + "<mo>&#x0221a;</mo>";
		return tokArray.slice(1);
		break;
	    case 'top':
		mmlString = mmlString + "<mo>&#x022a4;</mo>";
		return tokArray.slice(1);
		break;
	    case 'bot':
		mmlString = mmlString + "<mo>&#x022a5;</mo>";
		return tokArray.slice(1);
		break;
	    case 'angle':
		mmlString = mmlString + "<mo>&#x02220;</mo>";
		return tokArray.slice(1);
		break;
	    case 'triangle':
		mmlString = mmlString + "<mo>&#x025b3;</mo>";
		return tokArray.slice(1);
		break;
	    case 'backslash':
		mmlString = mmlString + "<mo>&#x02216;</mo>";
		return tokArray.slice(1);
		break;
	    case 'forall':
		mmlString = mmlString + "<mo>&#x02200;</mo>";
		return tokArray.slice(1);
		break;
	    case 'exists':
		mmlString = mmlString + "<mo>&#x02203;</mo>";
		return tokArray.slice(1);
		break;
	    case 'neg':
		mmlString = mmlString + "<mo>&#x000ac;</mo>";
		return tokArray.slice(1);
		break;
	    case 'wp':
		mmlString = mmlString + "<mi>&#x02118;</mi>";
		return tokArray.slice(1);
		break;
	    case 'flat':
		mmlString = mmlString + "<mo>&#x0266d;</mo>";
		return tokArray.slice(1);
		break;
	    case 'natural':
		mmlString = mmlString + "<mo>&#x0266e;</mo>";
		return tokArray.slice(1);
		break;
	    case 'sharp':
		mmlString = mmlString + "<mo>&#x0266f;</mo>";
		return tokArray.slice(1);
		break;
	    case 'clubsuit':
		mmlString = mmlString + "<mi>&#x02663;</mi>";
		return tokArray.slice(1);
		break;
	    case 'diamondsuit':
		mmlString = mmlString + "<mi>&#x02662;</mi>";
		return tokArray.slice(1);
		break;
	    case 'heartsuit':
		mmlString = mmlString + "<mi>&#x02661;</mi>";
		return tokArray.slice(1);
		break;
	    case 'spadesuit':
		mmlString = mmlString + "<mi>&#x02664;</mi>";
		return tokArray.slice(1);
		break;
	    case '':
		mmlString = mmlString + "<mi>&#x;</mi>";
		return tokArray.slice(1);
		break;
	    case '':
		mmlString = mmlString + "<mi>&#x;</mi>";
		return tokArray.slice(1);
		break;
	    case '':
		mmlString = mmlString + "<mi>&#x;</mi>";
		return tokArray.slice(1);
		break;
	    case 'leq':
		mmlString = mmlString + "<mo>&#x02264;</mo>";
		return tokArray.slice(1);
		break;
	    case 'geq':
		mmlString = mmlString + "<mo>&#x02267;</mo>";
		return tokArray.slice(1);
		break;
	    case 'equiv':
		mmlString = mmlString + "<mo>&#x02261;</mo>";
		return tokArray.slice(1);
		break;
	    case 'prec':
		mmlString = mmlString + "<mo>&#x0227a;</mo>";
		return tokArray.slice(1);
		break;
	    case 'succ':
		mmlString = mmlString + "<mo>&#x227b;</mo>";
		return tokArray.slice(1);
		break;
	    case 'sim':
		mmlString = mmlString + "<mo>&#x0223c;</mo>";
		return tokArray.slice(1);
		break;
	    case 'preceq':
		mmlString = mmlString + "<mo>&#x0227c;</mo>";
		return tokArray.slice(1);
		break;
	    case 'succeq':
		mmlString = mmlString + "<mo>&#x0227d;</mo>";
		return tokArray.slice(1);
		break;
	    case 'simeq':
		mmlString = mmlString + "<mo>&#x02243;</mo>";
		return tokArray.slice(1);
		break;
	    case 'll':
		mmlString = mmlString + "<mo>&#x0226a;</mo>";
		return tokArray.slice(1);
		break;
	    case 'gg':
		mmlString = mmlString + "<mo>&#x0226b;</mo>";
		return tokArray.slice(1);
		break;
	    case 'asymp':
		mmlString = mmlString + "<mo>&#x0224d;</mo>";
		return tokArray.slice(1);
		break;
	    case 'subset':
		mmlString = mmlString + "<mo>&#x02282;</mo>";
		return tokArray.slice(1);
		break;
	    case 'supset':
		mmlString = mmlString + "<mo>&#x02283;</mo>";
		return tokArray.slice(1);
		break;
	    case 'approx':
		mmlString = mmlString + "<mo>&#x02248;</mo>";
		return tokArray.slice(1);
		break;
	    case 'subseteq':
		mmlString = mmlString + "<mo>&#x02286;</mo>";
		return tokArray.slice(1);
		break;
	    case 'supseteq':
		mmlString = mmlString + "<mo>&#x02287;</mo>";
		return tokArray.slice(1);
		break;
	    case 'cong':
		mmlString = mmlString + "<mo>&#x02245;</mo>";
		return tokArray.slice(1);
		break;
	    case 'sqsubseteq':
		mmlString = mmlString + "<mo>&#x02291;</mo>";
		return tokArray.slice(1);
		break;
	    case 'sqsupseteq':
		mmlString = mmlString + "<mo>&#x02292;</mo>";
		return tokArray.slice(1);
		break;
	    case 'sqsubset':
		mmlString = mmlString + "<mo>&#x0228f;</mo>";
		return tokArray.slice(1);
		break;
	    case 'sqsupset':
		mmlString = mmlString + "<mo>&#x02290;</mo>";
		return tokArray.slice(1);
		break;
	    case 'bowtie':
		mmlString = mmlString + "<mo>&#x022c8;</mo>";
		return tokArray.slice(1);
		break;
	    case 'in':
		mmlString = mmlString + "<mo>&#x02208;</mo>";
		return tokArray.slice(1);
		break;
	    case 'ni':
		mmlString = mmlString + "<mo>&#x0220b;</mo>";
		return tokArray.slice(1);
		break;
	    case 'propto':
		mmlString = mmlString + "<mo>&#x0221d;</mo>";
		return tokArray.slice(1);
		break;
	    case 'vdash':
		mmlString = mmlString + "<mo>&#x022a2;</mo>";
		return tokArray.slice(1);
		break;
	    case 'dashv':
		mmlString = mmlString + "<mo>&#x022a3;</mo>";
		return tokArray.slice(1);
		break;
	    case 'models':
		mmlString = mmlString + "<mo>&#x022a8;</mo>";
		return tokArray.slice(1);
		break;
	    case 'smile':
		mmlString = mmlString + "<mo>smile</mo>";
		return tokArray.slice(1);
		break;
	    case 'frown':
		mmlString = mmlString + "<mo>frown</mo>";
		return tokArray.slice(1);
		break;
	    case 'mid':
		mmlString = mmlString + "<mo>&#x02758;</mo>";
		return tokArray.slice(1);
		break;
	    case 'parallel':
		mmlString = mmlString + "<mo>&#x02016;</mo>";
		return tokArray.slice(1);
		break;
	    case 'doteq':
		mmlString = mmlString + "<mo>&#x02250;</mo>";
		return tokArray.slice(1);
		break;
	    case 'perp':
		mmlString = mmlString + "<mo>perp</mo>";
		return tokArray.slice(1);
		break;
	    case '':
		return;
		break;
	    case 'cos':
		mmlString = mmlString + "<mo>cos</mo>";
		return tokArray.slice(1);
		break;
	    case 'sin':
		mmlString = mmlString + "<mo>sin</mo>";
		return tokArray.slice(1);
		break;
	    case 'tan':
		mmlString = mmlString + "<mo>tan</mo>";
		return tokArray.slice(1);
		break;
	    case 'csc':
		return ;
		break;
	    case 'sec':
		return ;
		break;
	    case 'cot':
		return ;
		break;
	    case 'arccos':
		return ;
		break;
	    case 'arcsin':
		return ;
		break;
	    case 'arctan':
		return ;
		break;
	    case 'cosh':
		return ;
		break;
	    case 'sinh':
		return ;
		break;
	    case 'tanh':
		return ;
		break;
	    case 'coth':
		return ;
		break;
	    case 'exp':
		return ;
		break;
	    case 'log':
		return ;
		break;
	    case 'arg':
		return ;
		break;
	    case 'deg':
		return ;
		break;
	    case 'det':
		return ;
		break;
	    case 'dim':
		return ;
		break;
	    case 'gcd':
		return ;
		break;
	    case 'hom':
		return ;
		break;
	    case 'inf':
		return ;
		break;
	    case 'ker':
		return ;
		break;
	    case 'lg':
		return ;
		break;
	    case 'lim':
		return ;
		break;
	    case 'liminf':
		return ;
		break;
	    case 'limsup':
		return ;
		break;
	    case 'ln':
		return ;
		break;
	    case 'max':
		return ;
		break;
	    case 'min':
		return ;
		break;
	    case 'Pr':
		return ;
		break;
	    case 'sup':
		return ;
		break;
// Greek
	    case 'Alpha':
		mmlString = mmlString + "<mi>&#x;</mi>";
		return tokArray.slice(1);
		break;
	    case 'alpha':
		mmlString = mmlString + "<mi>&#x003b1;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Beta':
		return ;
		break;
	    case 'beta':
		mmlString = mmlString + "<mi>&#x003b2;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Gamma':
		mmlString = mmlString + "<mi>&#x00393;</mi>";
		return tokArray.slice(1);
		break;
	    case 'gamma':
		mmlString = mmlString + "<mi>&#x003b3;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Delta':
		mmlString = mmlString + "<mi>&#x00394;</mi>";
		return tokArray.slice(1);
		break;
	    case 'delta':
		mmlString = mmlString + "<mi>&#x003b4;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Epsilon':
		return ;
		break;
	    case 'epsilon':
		mmlString = mmlString + "<mi>&#x003b5;</mi>";
		return tokArray.slice(1);
		break;
	    case 'varepsilon':
		return;
		break;
	    case 'Zeta':
		return ;
		break;
	    case 'zeta':
		mmlString = mmlString + "<mi>&#x003b6;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Eta':
		return ;
		break;
	    case 'eta':
		mmlString = mmlString + "<mi>&#x003b7;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Theta':
		mmlString = mmlString + "<mi>&#x00398;</mi>";
		return tokArray.slice(1);
		break;
	    case 'theta':
		mmlString = mmlString + "<mi>&#x003b8;</mi>";
		return tokArray.slice(1);
		break;
	    case 'vartheta':
		return;
		break;
	    case 'Iota':
		return ;
		break;
	    case 'iota':
		mmlString = mmlString + "<mi>&#x003b9;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Kappa':
		return ;
		break;
	    case 'kappa':
		mmlString = mmlString + "<mi>&#x003ba;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Lambda':
		mmlString = mmlString + "<mi>&#x0039b;</mi>";
		return tokArray.slice(1);
		break;
	    case 'lambda':
		mmlString = mmlString + "<mi>&#x003bb;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Mu':
		return ;
		break;
	    case 'mu':
		mmlString = mmlString + "<mi>&#x003bc;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Nu':
		return ;
		break;
	    case 'nu':
		mmlString = mmlString + "<mi>&#x003bd;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Xi':
		mmlString = mmlString + "<mi>&#x0039e;</mi>";
		return tokArray.slice(1);
		break;
	    case 'xi':
		mmlString = mmlString + "<mi>&#x003be;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Omicron':
		return ;
		break;
	    case 'omicron':
		return ;
		break;
	    case 'Pi':
		mmlString = mmlString + "<mi>&#x003a0;</mi>";
		return tokArray.slice(1);
		break;
	    case 'pi':
		mmlString = mmlString + "<mi>&#x003c0;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Rho':
		return ;
		break;
	    case 'rho':
		mmlString = mmlString + "<mi>&#x003c1;</mi>";
		return tokArray.slice(1);
		break;
	    case 'varrho':
		mmlString = mmlString + "<mi>&#x003f1;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Sigma':
		mmlString = mmlString + "<mi>&#x003f3;</mi>";
		return tokArray.slice(1);
		break;
	    case 'sigma':
		mmlString = mmlString + "<mi>&#x003c3;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Tau':
		return ;
		break;
	    case 'tau':
		mmlString = mmlString + "<mi>&#x003c4;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Upsilon':
		mmlString = mmlString + "<mi>&#x003a5;</mi>";
		return tokArray.slice(1);
		break;
	    case 'upsilon':
		mmlString = mmlString + "<mi>&#x003c5;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Phi':
		mmlString = mmlString + "<mi>&#x003a6;</mi>";
		return tokArray.slice(1);
		break;
	    case 'phi':
		mmlString = mmlString + "<mi>&#x003c6;</mi>";
		return tokArray.slice(1);
		break;
	    case 'varphi':
		mmlString = mmlString + "<mi>&#x003d5;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Chi':
		return ;
		break;
	    case 'chi':
		mmlString = mmlString + "<mi>&#x003c7;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Psi':
		mmlString = mmlString + "<mi>&#x003a8;</mi>";
		return tokArray.slice(1);
		break;
	    case 'psi':
		mmlString = mmlString + "<mi>&#x003c8;</mi>";
		return tokArray.slice(1);
		break;
	    case 'Omega':
		mmlString = mmlString + "<mi>&#x003a9;</mi>";
		return tokArray.slice(1);
		break;
	    case 'omega':
		mmlString = mmlString + "<mi>&#x003c9;</mi>";
		return tokArray.slice(1);
		break;
	    case '':
		return ;
		break;
	    case '':
		return ;
		break;
	    case '':
		return ;
		break;
	    case '':
		return ;
		break;
	    case '':
		return ;
		break;
	    case '':
		return ;
		break;
	    case '':
		return ;
		break;
	    case '':
		return ;
		break;
	    case '':
		return ;
		break;
	}
	mmlString = mmlString + tmp;
	return tokArray.slice(1);
    }

    function backSymFormat(itok,tokArray) {
	//alert('backSymFormat '+tokArray[itok][0]);
	mmlString = mmlString + "<mo>" + tokArray[itok][0] + "</mo>";
	return tokArray.slice(1);
    }

/* 
 * Backslash operator functions.  These are for operators that consume
 * more than one token.
 */

    function backCos(itok,tokArray) {
	mmlString = mmlString + "<com/>";
	return tokArray.slice(1);
    }


/*
  parseTex takes the tex string and returns an array of tokens.
  Tokens are of the form:
  1. control words begin with a \ followed by any number of letters
  the control word is terminated by a non letter. Spaces after
  control words terminate the control word but are ignored otherwise.
  2. control symbols begin with a \ followed by exactly one non letter
  3. variables for now will begin with a letter and then any sequence
  of letters and numbers
  4. single operators, + - * / ^ _
  5. blocks delimited by { }, these require recursing

  To determine a token the first character is needed and then subsequent
  characters are examined until the end of the token is reached

*/


    function trim(string) {
	////alert('trim start:' + string + ':');
	// trim leading and trailing white space
	string = string.replace(/\s*$/,'');
	string = string.replace(/^\s*/,'');
	////alert('trim end');
	return string;
    }

/*
 * tokArray is, so far, an array of length 3 subarrays
 * of form [token, token type, rank]
 */

    function parseTex(texString) {
	//alert('parseTex begin:'+texString+':');
	var  tokArray = new Array();
	// peel tokens off front of string
	var execArray;
	while ( texString != '') {
	    if ( execArray = variable.exec(texString) ) {
		//alert('parseTex variable: ' + texString);
		texString = texString.replace(variable,'');
		tokArray.push([execArray[1],'variable',0]);
	    }
	    else if ( execArray = number.exec(texString) ) {
		//alert('parseTex number: ' + texString);
		texString = texString.replace(number,'');
		tokArray.push([execArray[1],'number',0]);
	    }
	    // -+*/_^
	    else if ( execArray = single.exec(texString) ) {
		//alert('parseTex single: ' + texString);
		texString = texString.replace(single,'');
		switch(execArray[1]) {
		    case '-':
			tokArray.push([execArray[1],'operator',0]);
			break;
		    case '+':
			tokArray.push([execArray[1],'operator',0]);
			break;
		    case '*':
			tokArray.push([execArray[1],'operator',0]);
			break;
		    case '/':
			tokArray.push([execArray[1],'operator',1]);
			break;
		    case '_':
			tokArray.push([execArray[1],'operator',1]);
			break;
		    case '^':
			tokArray.push([execArray[1],'operator',1]);
			break;
		    case '(':
			tokArray.push([execArray[1],'operator',0]);
			break;
		    case ')':
			tokArray.push([execArray[1],'operator',0]);
			break;
		    case '=':
			tokArray.push([execArray[1],'operator',0]);
			break;
		}
	    }
	    else if ( execArray = backWord.exec(texString) ) {
		//alert('parseTex backWord: ' + texString);
		texString = texString.replace(backWord,'');
		switch(execArray[1]) {
		    case 'over':
			tokArray.push([execArray[1],'backWord',-1]);
			break;
		    default:
			tokArray.push([execArray[1],'backWord',0]);
			break;
		}
	    }
	    else if ( execArray =  backSym.exec(texString) ) {
		//alert('parseTex backSym: ' + texString);
		texString = texString.replace(backSym,'');
		tokArray.push([execArray[1],'backSym',0]);
	    }
	    else if ( texString[0] == '{' ) {
		//alert('parseTex brace: ' + texString);
		var next = forBrace(texString);
		//alert('parseTex brace next: ' + next);
		//alert('parseTex brace token: ' + texString.slice(0,next));
		tokArray.push([texString.slice(0,next),'brace',0]);
		if (next == texString.length) texString = '';
		else texString = texString.slice(next);
	    }
	    else {
		//alert('parseTex none: '+ texString);
		//first token not recognized
		texString = '';
	    }
	    texString = trim(texString);
	}
	return tokArray;
    }

}

/*
 * forBrace takes a string starting with a '{' and finds
 * the matching '}'.  It returns the position of the closing
 * brace plus 1.
 */

function forBrace(string) {
    var level = 1;
    var pos = 1;
    while ( level > 0 ) {
	if (string[pos] == '{') level++;
	if (string[pos] == '}') level--;
	pos++;
    }
    return pos;
}
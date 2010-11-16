/*
 * module for manipulating CSS classes.  Thanks to David Flanagan, 'Javascript The
 * Definitive Guide' O'Reilly for these functions
 */

var CSSClass = {};

// Return true if element e is a member of the class c; false otherwise

CSSClass.is = function(e,c) {
    if (typeof e == "string") e = document.getElementById(e);
    //optimize common cases
    var classes = e.className;
    if (!classes) return false;  // Not a member of any classes
    if (classes == c) return true; // Member of just this one class
    return e.className.search("\\b" + c + "\\b") != -1;
}

// Add class c to the className of element e if it is not already there.

CSSClass.add = function(e,c) {
    if (typeof e == "string") e = document.getElementById(e);
    if (CSSClass.is(e,c)) return; // If already a member do nothing
    if (e.className) c = " " + c; // Whitespace separator if needed
    e.className += c;  // Append the new class to the end
}

// Remove all occurrences of class c from the className of element e

CSSClass.remove = function(e,c) {
    if (typeof e == "string") e = document.getElementById(e);
    e.className = e.className.replace(new RegExp("\\b" + c + "\\b\\s*","g"), "");
}

function keyPressed(e) {
    var keynum, keychar, shift;
    targ = e.target;
    var command = targ.value;
    keynum = e.which
    shift = e.shiftKey;
    if(shift && keynum == 13 && command != "") {
        makeRequest(targ);
    }
}

/* Note that since this is Firefox specific I don't have to
 * bother with trying different AJAX request objects.
 */

function makeRequest(targ) {
    var command = targ.value;
    http_request = new XMLHttpRequest();	 
    http_request.open('POST', 'http://127.0.0.1:8085', true);
    http_request.onreadystatechange = addCompCell;
    http_request.setRequestHeader('Content-Type', 'text/plain');
    http_request.send("command="+command);
}


function addCompCell() {
    if (http_request.readyState == 4) {
	if (http_request.status == 200) {
	    var mathString = http_request.responseText;
            var parent = ctgCompCell(targ);
            targ.value = targ.getAttribute('commSave');
            if ( CSSClass.is(parent,'rootCompCell') ) {
                parent.insertBefore(makeCompCell(mathString),getFirstCompCell(parent));                
            } else if ( CSSClass.is(parent,'compCell') ) {
	        parent.insertBefore(makeCompCell(mathString),getResultBox(parent));
            }

            //var ansBox = document.getElementById('mathAns');
            //ansBox.insertBefore(makeCompCell(mathString),ansBox.firstChild);

            
	}
    }
}

function ctgCompCell(targ) {
    // given any node this should return the containing compCell
  if (CSSClass.is(targ.parentNode,'compCell') || CSSClass.is(targ.parentNode,'rootCompCell')) {
	return targ.parentNode;
    }
    else {
	return ctgCompCell(targ.parentNode);
    }
}

function getFirstCompCell(compCell) {
    var children = compCell.childNodes;
    for (var i = 0; i < children.length; i++) {
	if (CSSClass.is(children[i],'compCell')) return children[i];
    }
    return null;
}

/*
 * Each compCell has a unique resultBox determined by its className
 */

function getResultBox(compCell) {
    var children = compCell.childNodes;
    for (var i = 0; i < children.length; i++) {
	if (CSSClass.is(children[i], 'result')) return children[i];
    }
}


/*
 * mathString delivered from panserver has form:
 * <div class="stepnum">...</div>
 * <div class="command">...</div>
 * <div class="algebra">...</div>
 * <div class="mathml">...</div>
 * <div class="type">...</div>
 * In makeCompCell I convert the html elements to
 * xul elements and add the necessary decorations.
 * The resulting compCell looks like:
 * <vbox class="compCell">
 *   <hbox>
 *     <vbox>
 *       <spacer height="..."/>
 *       <label value="(stepNum) ->"/>
 *     </vbox>
 *     <textbox value="inComm"/>
 *   </hbox>
 *   <vbox class="result">
 *     <box class="stepnum" value="stepNum"/>
 *     <box class="command" value="inComm"/>
 *     <box class="algebra" value="algebra"/>
 *     <hbox style="...">
 *       <math ...>...</math>
 *     </hbox>
 *     <description class="type" value="type"/>
 *       .
 *       .
 *       child compCells
 *       .
 *       .
 *   </vbox>
 *   <spacer height="..." flex="..."/>
 * </vbox>
 * Note that the 'box' element does not have a standard 'value'
 * attribute so nothing is actually displayed when setting a 
 * value attribute on a box element.  This avoids setting
 * style="display: none;".
 */


function makeCompCell(mathString) {
    var compCell = document.createElement('vbox');
    compCell.style.border = 'solid 1px blue';
    compCell.setAttribute('class','compCell');
    compCell.style.marginTop = '5px';
    compCell.style.marginBottom = '5px';
    var resultBox = document.createElement('vbox');
    resultBox.setAttribute('class','result');
    var mathRange = document.createRange();
    mathRange.selectNodeContents(resultBox);
    var mathFragment = mathRange.createContextualFragment(mathString);
    resultBox.appendChild(mathFragment);
    var stepBox = getStepBox(resultBox);
    var commBox = getCommandBox(resultBox);
    var algBox = getAlgebraBox(resultBox);
    var mathBox = getMathmlBox(resultBox);
    var typeBox = getTypeBox(resultBox);
    var stepNum = stepBox.firstChild.data;
    var inComm = commBox.firstChild.data;
    if (algBox.firstChild) var algebra = algBox.firstChild.data;
    else var algebra = "";
    var mathml = mathBox.firstChild;
    var type = typeBox.firstChild.data;
    var nStepBox = document.createElement('box');
    nStepBox.setAttribute('class','stepnum');
    nStepBox.setAttribute('value',stepNum);
    resultBox.insertBefore(nStepBox,stepBox);
    resultBox.removeChild(stepBox);
    var nCommBox = document.createElement('box');
    nCommBox.setAttribute('class','command');
    nCommBox.setAttribute('value',inComm);
    resultBox.insertBefore(nCommBox,commBox);
    resultBox.removeChild(commBox);
    var nAlgBox = document.createElement('box');
    nAlgBox.setAttribute('class','algebra');
    nAlgBox.setAttribute('value',algebra);
    resultBox.insertBefore(nAlgBox,algBox);
    resultBox.removeChild(algBox);
    var nMathBox = document.createElement('hbox');
    nMathBox.style.paddingLeft = "100px";
    resultBox.removeChild(mathBox);
    nMathBox.appendChild(mathBox.firstChild);
    resultBox.insertBefore(nMathBox,typeBox);
    var nTypeBox = document.createElement('description');
    nTypeBox.setAttribute('class','type');
    nTypeBox.setAttribute('value',type);
    resultBox.insertBefore(nTypeBox,typeBox);
    resultBox.removeChild(typeBox);
    //
    var inBox = document.createElement('hbox');
    var stepBox = document.createElement('label');
    stepBox.setAttribute('value','('+stepNum+') -> ');
    var stepCtgBox = document.createElement('vbox');
    var stepSpacer = document.createElement('spacer');
    stepSpacer.setAttribute('height','7');
    stepCtgBox.appendChild(stepSpacer);
    stepCtgBox.appendChild(stepBox);
    inBox.appendChild(stepCtgBox);
    var inCommBox = document.createElement('textbox');
    inCommBox.setAttribute('value',inComm);
    inCommBox.setAttribute('commSave',inComm);
    inCommBox.setAttribute('onkeypress','keyPressed(event)');
    inCommBox.style.width = '600px';
    inBox.appendChild(inCommBox);
    resultBox.insertBefore(inBox,resultBox.firstChild);
    //
    compCell.appendChild(resultBox);
    var vspace = document.createElement('spacer');
    vspace.setAttribute('height', '20');
    vspace.setAttribute('flex','1');
    compCell.appendChild(vspace);
    return compCell;
}





/* I don't want to rely on the specific structure of the resultBox to access
 * it's children.  Rather I want to think of it as a bag containing, at present,
 * boxes for stepnum, command, algebra, mathml, and type which could be in any
 * order.  Also I may want to add something new, for instance a TeX box.  So I
 * define functions to retrieve these nodes for a given resultBox node.
 */

function getStepBox(resultBox) {
    // make sure argument is of right class
    if (!CSSClass.is(resultBox,'result')) {
	alert('getStepBox: argument not resultBox');
	return;
    }
    var children = resultBox.childNodes;
    for (var i = 0; i < children.length; i++) {
	if (CSSClass.is(children[i],'stepnum')) return children[i]
    }
}

function getCommandBox(resultBox) {
    // make sure argument is of right class
    if (!CSSClass.is(resultBox,'result')) {
	alert('getStepBox: argument not resultBox');
	return;
    }
    var children = resultBox.childNodes;
    for (var i = 0; i < children.length; i++) {
	if (CSSClass.is(children[i],'command')) return children[i]
    }
}

function getAlgebraBox(resultBox) {
    // make sure argument is of right class
    if (!CSSClass.is(resultBox,'result')) {
	alert('getStepBox: argument not resultBox');
	return;
    }
    var children = resultBox.childNodes;
    for (var i = 0; i < children.length; i++) {
	if (CSSClass.is(children[i],'algebra')) return children[i]
    }
}


function getMathmlBox(resultBox) {
    // make sure argument is of right class
    if (!CSSClass.is(resultBox,'result')) {
	alert('getStepBox: argument not resultBox');
	return;
    }
    var children = resultBox.childNodes;
    for (var i = 0; i < children.length; i++) {
	if (CSSClass.is(children[i],'mathml')) return children[i]
    }
}

function getTypeBox(resultBox) {
    // make sure argument is of right class
    if (!CSSClass.is(resultBox,'result')) {
	alert('getStepBox: argument not resultBox');
	return;
    }
    var children = resultBox.childNodes;
    for (var i = 0; i < children.length; i++) {
	if (CSSClass.is(children[i],'type')) return children[i]
    }
}

function ctgCompCell(targ) {
    // given any node this should return the containing compCell
    if (targ.parentNode.className == 'compCell' || targ.parentNode.className == 'rootCompCell') {
	return targ.parentNode;
    }
    else {
	return ctgCompCell(targ.parentNode);
    }
}

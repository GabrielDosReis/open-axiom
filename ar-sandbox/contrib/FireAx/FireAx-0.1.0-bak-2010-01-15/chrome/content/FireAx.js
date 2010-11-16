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


/*
function keyPressed(e) {
  var keynum, keychar, shift;
  //  var command = document.getElementById ('commreq').value
  // this works in Firefox, do we need something else in IE?
    targ = e.target;
    command = targ.value;
    keynum = e.which
    shift = e.shiftKey;
    if(shift && keynum == 13 && command != "") {
        makeRequest(targ);
    }
}
*/

function keyPressed(e) {
  var keynum, keychar, shift;
    targ = e.target;
    command = targ.value;
    keynum = e.which
    shift = e.shiftKey;
    if(shift && keynum == 13 && command != "") {
        makeRequest();
    }
}


function makeRequest() {
    http_request = new XMLHttpRequest();	 
    var command = document.getElementById('axcom').value;
    http_request.open('POST', 'http://127.0.0.1:8085', true);
    http_request.onreadystatechange = handleResponse;
    http_request.setRequestHeader('Content-Type', 'text/plain');
    http_request.send("command="+command);
}



/*
 * mathString delivered from panserver has form:
 * <div class="stepnum">...</div>
 * <div class="command">...</div>
 * <div class="algebra">...</div>
 * <div class="mathml">...</div>
 * <div class="type">...</div>
 */


function handleResponse() {
    if (http_request.readyState == 4) {
	if (http_request.status == 200) {
	    var mathString = http_request.responseText;
            makeCompCell(mathString);
	}
    }
}


function makeCompCellBak0(mathString) {
    var compCell = document.createElement('vbox');
    compCell.setAttribute('class','compCell');
    var resultBox = document.createElement('vbox');
    resultBox.setAttribute('class','result');
    var mathRange = document.createRange();
    mathRange.selectNodeContents(resultBox);
    var mathFragment = mathRange.createContextualFragment(mathString);
    resultBox.appendChild(mathFragment);
    resultBox.firstChild.style.display = 'none';
    resultBox.firstChild.nextSibling.style.display = 'none';
    var stepNum = resultBox.firstChild.firstChild.data;
    var inComm = resultBox.firstChild.nextSibling.firstChild.data;
    /*
    var inCommBox = document.createElement('description');
    var inCommBoxText = '('+stepNum+') -> '+inComm;
    inCommBox.setAttribute('value',inCommBoxText);
    */
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
    inBox.appendChild(inCommBox);
    resultBox.insertBefore(inBox,resultBox.firstChild);
    var mathBox = getMathmlBox(resultBox);
    var mathSpace = document.createElement('spacer');
    mathSpace.setAttribute('height','10');
    resultBox.insertBefore(mathSpace,mathBox);
    var typeBox = getTypeBox(resultBox);
    var typeSpace = document.createElement('spacer');
    typeSpace.setAttribute('height','10');
    resultBox.insertBefore(typeSpace,typeBox);
    compCell.appendChild(resultBox);
    var ansBox = document.getElementById('mathAns');
    ansBox.insertBefore(compCell,ansBox.firstChild);
    var vspace = document.createElement('spacer');
    vspace.setAttribute('height', '20');
    vspace.setAttribute('flex','1');
    compCell.appendChild(vspace);
}

function makeCompCell(mathString) {
    var compCell = document.createElement('vbox');
    compCell.setAttribute('class','compCell');
    compCell.appendChild(makeResultBox(mathString));
    var ansBox = document.getElementById('mathAns');
    ansBox.insertBefore(compCell,ansBox.firstChild);
    var vspace = document.createElement('spacer');
    vspace.setAttribute('height', '20');
    vspace.setAttribute('flex','1');
    compCell.appendChild(vspace);
}

/*
 * mathString delivered from panserver has form:
 * <div class="stepnum">...</div>
 * <div class="command">...</div>
 * <div class="algebra">...</div>
 * <div class="mathml">...</div>
 * <div class="type">...</div>
 */

/*
 * In 'makeResultBox' I change the html 'div' elements
 * to xul elements.
 */

function makeResultBox(mathString){
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
    inBox.appendChild(inCommBox);
    resultBox.insertBefore(inBox,resultBox.firstChild);
    return resultBox;
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



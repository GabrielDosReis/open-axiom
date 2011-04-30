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

/* End CSSClass defs */

/* Module to drag an element
 * 
 * Arguments:
 *
 *  elementToDrag: the element that received the mousedown event or
 *    some containing element.  It must be absolutely positioned.  Its
 *    style.left and style.top values will be changed based on the user's
 *    drag.
 *
 *  event:  the Event object for the mousedown event.
 *
 */

function drag(elementToDrag, event){
    // The mouse position, in window coordinates, at which the 
    // drag event begins.
    var startX = event.clientX, startY = event.clientY;
    
    // The original position, in document coordinates, of the  element
    // that is going to be dragged.  Since elementToDrag is absolutely positioned
    // we assume that its offsetParent is the document body.
    var origX = elementToDrag.offsetLeft, origY = elementToDrag.offsetTop;

    // Even though the coordinates are computed in different coordinate systems
    // we can still compute the difference between them and use it in the 
    // moveHandler() function.  This works because the scrollbar position never
    // changes during the drag.
    var deltaX = startX - origX, deltaY = startY - origY;

    // Register the event handlers that will respond to the mousemove events
    // and the mouseup event that follow this mousedown event.
    document.addEventListener("mousemove",moveHandler,true);
    document.addEventListener("mouseup",upHandler,true);

    // We've handled this event.  Don't let anybody else see it.
    event.preventDefault();

    /*
     * This is the handler that captures mousemove events when an element is
     * being dragged.  It is responsible for moving the element.
     */

    function moveHandler(e) {
	// Move the element to the current mouse position adusted as necessary by
	// the offset of the initial mouse click.
	elementToDrag.style.left = (e.clientX - deltaX) + "px";
	elementToDrag.style.top = (e.clientY - deltaY) + "px";
	// Don't let anyone else see this event.
	e.stopPropagation();
    }

    /*
     * This is the handler that captures the final mouseup event that occurs at
     * the end of a drag.
     */

    function upHandler(e) {
	// Unregister the capturing event handlers.
	document.removeEventListener("mouseup",upHandler,true);
	document.removeEventListener("mousemove",moveHandler,true);
	// Don't let the event propagate any further.
	e.stopPropagation();
    }
}

/* End drag functions */


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
    CSSClass.add(compCell,'compCell');
    var spaceBox = document.createElement('box');
    spaceBox.setAttribute('height','5');
    spaceBox.setAttribute('onclick','showCellContext(event)');
    compCell.appendChild(spaceBox);
    var resultBox = document.createElement('vbox');
    CSSClass.add(resultBox,'result');
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
    CSSClass.add(nStepBox,'stepnum');
    nStepBox.setAttribute('value',stepNum);
    resultBox.insertBefore(nStepBox,stepBox);
    resultBox.removeChild(stepBox);
    var nCommBox = document.createElement('box');
    CSSClass.add(nCommBox,'command');
    nCommBox.setAttribute('value',inComm);
    resultBox.insertBefore(nCommBox,commBox);
    resultBox.removeChild(commBox);
    var nAlgBox = document.createElement('box');
    CSSClass.add(nAlgBox,'algebra');
    nAlgBox.setAttribute('value',algebra);
    resultBox.insertBefore(nAlgBox,algBox);
    resultBox.removeChild(algBox);
    var nMathBox = document.createElement('box');
    CSSClass.add(nMathBox,'mathml');
    nMathBox.setAttribute('onclick','showCellContext(event)');
    //nMathBox.setAttribute('popup','cellmenu');
    resultBox.removeChild(mathBox);
    nMathBox.appendChild(mathBox.firstChild);
    resultBox.insertBefore(nMathBox,typeBox);
    var nTypeBox = document.createElement('description');
    CSSClass.add(nTypeBox,'type');
    nTypeBox.setAttribute('onclick','showCellContext(event)');
    //nTypeBox.setAttribute('popup','cellmenu');
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
    CSSClass.add(inCommBox,'inComm');
    inCommBox.setAttribute('value',inComm);
    inCommBox.setAttribute('commSave',inComm);
    inCommBox.setAttribute('onkeypress','keyPressed(event)');
    inBox.appendChild(inCommBox);
    resultBox.insertBefore(inBox,resultBox.firstChild);
    //
    compCell.appendChild(resultBox);
    var vspace = document.createElement('spacer');
    vspace.setAttribute('height', '20');
    vspace.setAttribute('flex','1');
    resultBox.appendChild(vspace);
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


function ctgResultBox(targ) {
    if ( CSSClass.is(targ,'result') ) return targ;
    else return ctgResultBox(targ.parentNode);
}

/* functions for popup context menu */

function showCellContext(event) {
    gtarg = event.target;
    var popup = document.getElementById('cellmenu');
    popup.openPopup(event.target,"after_pointer",0,0,false,false);
}

function showCell(event) {
  var g = getResultBox(ctgCompCell(gtarg));
    if (CSSClass.is(g,'hide') ) CSSClass.remove(g,'hide');
    else CSSClass.add(g,'hide');
}

function showBranch(event) {
    var compCell = ctgCompCell(gtarg);
    var children = compCell.childNodes;
    for ( var i = 0; i < children.length; i++) {
      if ( CSSClass.is(children[i],'compCell') || CSSClass.is(children[i],'result') ) {
	    if (CSSClass.is(children[i],'hide')) {
		CSSClass.remove(children[i],'hide');
	    } else {
		CSSClass.add(children[i],'hide');
	    }
      }
    }
}

/*
 *  I want to test to see if the present compCell has any children
 *  and, if not, then I'm at the head of the branch and I do nothing,
 *  but if it does then apply showHead recursively and hide the present
 *  cell.  First thing is I need a list of the compCell children.
 */

function showHead(event) {
    var compCell = ctgCompCell(gtarg);
    var compCells = new Array();
    var children = compCell.childNodes;
    for ( var i = 0; i < children.length; i++) {
	if ( CSSClass.is(children[i],'compCell') ) {
	    compCells.push(children[i]);
	}
    }
    if ( compCells.length > 0 ) { //if there are children compCells then hide contents
	for ( i = 0; i < children.length; i++) {
	    if ( !CSSClass.is(children[i],'compCell') ) {
		if (CSSClass.is(children[i],'hide')) {
		    CSSClass.remove(children[i],'hide');
		} else {
		    CSSClass.add(children[i],'hide');
		}
	    }
	}
    }
}

/*  Get list of sibling compCells, if more than one
 *  then cyclically rotate the present one to first
 */

function rotateHead(compCell) {
    var parent = compCell.parentNode;
    var sibCompCells = getSibCompCells(compCell);
    var j;
    // also need to handle case when there are no sibling compCells
    // but still want to rotate with result box of ctgCompCell
    if ( sibCompCells.length > 1 ) { //do the rotation
	// find index of compCell among siblings
	for ( i = 0; i < sibCompCells.length; i++) {
	    if ( sibCompCells[i] == compCell ) j = i; 
	}
	// j is index of compCell, if j != 0 rotate to 0
	var swap;
	for ( i = 0; i < j; i++) {
	    parent.appendChild(parent.removeChild(sibCompCells[i]));
	}
    }
}


function showContext(e) {
    var x = e.clientX;
    var y = e.clientY;
    x = x + window.pageXOffset;
    y = y + window.pageYOffset;
    // set the containing compCell and make sure event doesn't arise
    // from a command box
    var f = e.target;
    while ( !f.className ) {
	f = f.parentNode;
    }
    // pick which classes of elements get this context menu
    if ( !(CSSClass.is(f,'mathml') || CSSClass.is(f,'algebra') || CSSClass.is(f,'type') || CSSClass.is(f,'compCell') || CSSClass.is(f,'rootCompCell'))) return;
    if (f.className == 'compCell') var g = f;
    else var g = ctgCompCell(f);
    // use an anonymous function to hold everything else
    (function() {
	var contents = document.getElementById('main');
	// create box to hold the menu
	var menuBox = document.createElementNS('http://www.w3.org/1999/xhtml','div');
	// create a drag bar at the top
	var dragBar = document.createElementNS('http://www.w3.org/1999/xhtml','div');
	menuBox.appendChild(dragBar);
	dragBar.setAttribute('style','background-color: gray; height: 10px; width: 100px;');
	dragBar.setAttribute('onmousedown','drag(this.parentNode,event)');
/*
 * set the class on the menuBox so it can be styled in mainstyle.css
 * and position it
 */
	menuBox.setAttribute('class','context-menu');
	menuBox.setAttribute('style','position: absolute; left: ' + x + 'px; top: ' + y + 'px;');
/*
 * attach everything to document
 */
	contents.appendChild(menuBox);
	menuBox.scrollIntoView();
        
    })();
}

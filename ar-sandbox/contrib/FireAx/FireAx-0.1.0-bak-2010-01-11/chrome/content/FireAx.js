function makeRequest() {
    http_request = new XMLHttpRequest();	 
    var command = document.getElementById("axcom").value;
    http_request.open('POST', 'http://127.0.0.1:8085', true);
    http_request.onreadystatechange = handleResponse;
    http_request.setRequestHeader('Content-Type', 'text/plain');
    http_request.send("command="+command);
}

function handleResponse() {
    if (http_request.readyState == 4) {
	if (http_request.status == 200) {

// stick response in vbox=mathBox
	    var mathString = http_request.responseText;
            var mathRange = document.createRange();
	    var mathBox = document.createElement('vbox');
            mathRange.selectNodeContents(mathBox);
            var mathFragment = mathRange.createContextualFragment(mathString);
            mathBox.appendChild(mathFragment);
// set id on mathBox
	    var stepNum = mathBox.firstChild.firstChild.data;
	    mathBox.setAttribute('id', 'step'+stepNum);
	    // make 'stepnum' and 'command' boxes hidden
            mathBox.firstChild.style.display = 'none';
            mathBox.firstChild.nextSibling.style.display = 'none';
// get command and save it with an In label
// need to test for error, i.e. step number of -1 and do something different
	    if ( stepNum != "-1" )
	    {
		var commSave = document.createElement('description');
		var commSaveCont = document.createElement('hbox');
		var commSaveLabel = document.createElement('label');
		commSave.setAttribute('value',document.getElementById('axcom').value);
		commSaveLabel.setAttribute('value', ' In '+stepNum+':');
		commSaveCont.appendChild(commSaveLabel);
		commSaveCont.appendChild(commSave);
	    }
	    else
	    {
		var commError = document.createElement('description');
		var commSaveCont = document.createElement('hbox');
		commError.setAttribute('value', 'What sort of command is '+document.getElementById('axcom').value+'?');
		commSaveCont.appendChild(commError);
	    }
// add some space between the In and Out
	    var vspace = document.createElement('spacer');
	    vspace.setAttribute('height', '10');
	    vspace.setAttribute('flex', '1');
	    mathBox.insertBefore(vspace, mathBox.firstChild);
	    mathBox.insertBefore(commSaveCont, mathBox.firstChild);
	    var vspace1 = document.createElement('spacer');
	    vspace1.setAttribute('height', '15');
	    vspace1.setAttribute('flex', '1');
            mathBox.appendChild(vspace1);
// insert everything into the document
	    document.getElementById('mathAns').appendChild(mathBox);
//	    document.getElementById('main').insertBefore(mathBox,document.getElementById('combox'));
	    var tempElt = document.getElementById('mathAns').scrollBoxObject;
	    tempElt.ensureElementIsVisible(mathBox);
	} else {
	    alert('There was a problem with the request.'+ http_request.statusText);
	}
    }
}

function makeCompCell(mathString,parent) {

}

function saveWork() {
    var doc = document.getElementById('mathAns');
    var serializer = new XMLSerializer();
    var foStream = Components.classes["@mozilla.org/network/file-output-stream;1"]
	.createInstance(Components.interfaces.nsIFileOutputStream);
    var file = Components.classes["@mozilla.org/file/directory_service;1"]
	.getService(Components.interfaces.nsIProperties)
	.get("Home", Components.interfaces.nsIFile); // get home folder
    file.append("axiom_data");   // extensions sub-directory
    file.append("axSave.xml");   // filename
    foStream.init(file, 0x02 | 0x08 | 0x20, 0664, 0);   // write, create, truncate
    serializer.serializeToStream(doc, foStream, "");   // rememeber, doc is the DOM tree
    foStream.close();

}


function saveWork1(data) {
//    var data = "beauty";
    var foStream = Components.classes["@mozilla.org/network/file-output-stream;1"]
	.createInstance(Components.interfaces.nsIFileOutputStream);
    var file = Components.classes["@mozilla.org/file/local;1"]
	.createInstance(Components.interfaces.nsILocalFile);
    file.initWithPath("/home/arthur/test.txt");   // filename
    foStream.init(file, 0x02 | 0x08 | 0x20, 0664, 0);   // write, create, truncate
    foStream.write(data, data.length);
    foStream.close();
}

function restoreWork() {
/*
    var data = '<vbox xmlns=\"http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul\" id=\"shit\"><vbox id=\"box1\"><description>A</description></vbox><vbox><description>B</description></vbox><vbox><description>C</description></vbox></vbox>';
    var parser = new DOMParser();
    var dom = parser.parseFromString(data, "text/xml");
    var restoreNode = dom.getElementById('shit');
    var mathNode = document.getElementById('mathAns');
    var eltList = restoreNode.childNodes;
    while (eltList.length > 0)
    {
	mathNode.appendChild(eltList.item(0));
    }
*/

    var savData = "";
    var file = Components.classes["@mozilla.org/file/directory_service;1"]
	.getService(Components.interfaces.nsIProperties)
	.get("Home", Components.interfaces.nsIFile); // get home folder
    file.append("axiom_data");   // sub-directory
    file.append("axSave.xml");   // filename
    var fstream = Components.classes["@mozilla.org/network/file-input-stream;1"]
	.createInstance(Components.interfaces.nsIFileInputStream);
    var sstream = Components.classes["@mozilla.org/scriptableinputstream;1"]
	.createInstance(Components.interfaces.nsIScriptableInputStream);
    fstream.init(file, 1, 0, false);
    sstream.init(fstream); 

    var str = sstream.read(-1);
    while (str.length > 0) {
	savData += str;
	str = sstream.read(-1);
    }

    sstream.close();
    fstream.close();
    var parser = new DOMParser();
    var dom = parser.parseFromString(savData, "text/xml");
    var restoreNode = dom.getElementById('mathAns');
    var mathNode = document.getElementById('mathAns');
    var eltList = dom.getElementById('mathAns').childNodes;
    while ( eltList.length > 0)
    {
	mathNode.appendChild(eltList.item(0));
    }

}

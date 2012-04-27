// global declarations
var ie = (document.all) ? true : false;

// UI functions
function findPos(obj) {
    var curLeft = curTop = 0;
    if (obj.offsetParent) {
	do {
	    curLeft += obj.offsetLeft;
	    curTop += obj.offsetTop;
	} while (obj = obj.offsetParent);
    }
    return [curLeft,curTop];
}

function centerObj(obj) {
    var curLeft = parseInt(obj.style.left);
    var width = parseInt(obj.offsetWidth);
    curLeft -= width / 2;
    if (curLeft < 0) { curLeft = 0; }
    obj.style.left = curLeft + "px";
}

function hidePopups(e) {
    e = e || event;
    var target = e.target || e.srcElement;
    do {
	if (target.className == "popup") {
	    // Click occured inside the box, do nothing.
	    return;
	}
	target = target.parentNode;
    } while (target);
    // Click was outside the popup, hide it.
    var elements = (ie) ? document.all : document.getElementsByTagName('*');
    for (i=0; i<elements.length; i++) {
	if (elements[i].className == "popup"){
	    elements[i].style.display = "none";
	}
    }
    document.onclick = null;
};

function makePopup (popupId, anchorElement) {
    var popupElement = document.getElementById (popupId);
    var anchorPos = findPos (anchorElement);
    popupElement.parentNode.removeChild(popupElement);
    document.body.appendChild(popupElement);
    popupElement.style.left = anchorPos[0] + "px";
    popupElement.style.top = anchorPos[1] + "px";
    popupElement.style.display = "block";
    centerObj(popupElement);
    document.onclick = function() { document.onclick = hidePopups; };
}

// first define schoozNotify function
var schoozUpdate = function() { };

// now console loggers
var console_log = function(e) { puts(e); };
var dummy_log = function(e) { };
// console_log ("hi there, logging function seems to be working ");

// start interpreter
var intp = new BiwaScheme.Interpreter (console_log);

// function to run first action
var runFirstAction = function() {
//    console_log ("Running first action");
    intp.evaluate ("(schooz:js-call-initial-action)", dummy_log);
};

// load Scheme files
var schemeFiles = ["scm/core/schooz.scm",
		   "scm/core/machines.scm",
		   "scm/core/once.scm",
		   "scm/ui/js.scm",
//		   "scm/demo/rock.scm"];
		   "scm/demo/redpill.scm"];
var lastFunction = runFirstAction;
schemeFiles.reverse().forEach (function (theUrl) {
    // create local copies of variables, to force closures
    var theUrlLocal = theUrl;
    var lastFunctionLocal = lastFunction;
//    console_log ("creating xhr for " + theUrl);
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function() {
	if (xhr.readyState == 4) {
//	    console_log ("Loaded " + xhr.responseText.length + " bytes from " + theUrlLocal);
	    intp.evaluate (xhr.responseText, dummy_log);
	    lastFunctionLocal();
	}
    };
    lastFunction = function() {
//	console_log ("Loading " + theUrlLocal);
	xhr.open("GET", theUrlLocal, true);
	xhr.send(null);
    }
});
lastFunction();


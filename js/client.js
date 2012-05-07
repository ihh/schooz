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

    var nbr = obj.parentNode;

    var nbrPos = findPos (nbr);
    obj.style.left = nbrPos[0] + "px";
    obj.style.top = nbrPos[1] + "px";

    var curLeft = parseInt(obj.style.left);
    var objWidth = parseInt(obj.offsetWidth);
    var nbrWidth = parseInt(nbr.offsetWidth);
    var winWidth = document.body.clientWidth;
    curLeft += (nbrWidth - objWidth) / 2;
    if (curLeft < 0) { curLeft = 0; }
    if (curLeft > winWidth - objWidth) { curLeft = winWidth - objWidth; }
    obj.style.left = curLeft + "px";

    var curTop = parseInt(obj.style.top);
    var nbrHeight = parseInt(nbr.offsetHeight);
    var nbrPadding = parseInt(window.getComputedStyle(nbr,null).getPropertyValue("padding"));
    curTop += nbrHeight - nbrPadding - 1;  // ensure no space between elements
    obj.style.top = curTop + "px";
}

function hideIfEventOutsidePopup(e) {
    e = e || event;
    var target = e.target || e.srcElement;
    do {
	if (target.className == "popup") {
	    // Click occured inside the popup, do nothing.
	    return;
	}
	target = target.parentNode;
    } while (target);
    // Click was outside the popup, hide it.
    hideAllPopups();
}
function hideOnNextClickOutsidePopup() { document.onclick = hideIfEventOutsidePopup; }
function hideOnNextMouseOutsidePopup() { document.onmouseover = hideIfEventOutsidePopup; }

function hideAllPopups() {
    var elements = (ie) ? document.all : document.getElementsByTagName('*');
    for (i=0; i<elements.length; i++) {
	if (elements[i].className == "popup"){
	    elements[i].style.display = "none";
	}
    }
    document.onclick = null;
    document.onmouseover = null;
    if (popupAnchor) {
//	popupAnchor.style.borderStyle = "none";
//	popupAnchor.style.borderWidth = 0;
    }
    popupClicked = 0;
    popupAnchor = null;
};

function attachPopups() {
    hideAllPopups();
    var elements = (ie) ? document.all : document.getElementsByTagName('*');
    for (i=0; i<elements.length; i++) {
	if (elements[i].className == "popup") {
	    var popupElement = elements[i];
	    var anchorElement = document.getElementById (popupElement.id + "Link");
	    elements[i].parentNode.removeChild (popupElement);
	    if (anchorElement) {
		anchorElement.appendChild (popupElement);
	    }
	}
    }
    document.onclick = null;
    document.onmouseover = null;
};

function centerAllPopups() {
    var elements = (ie) ? document.all : document.getElementsByTagName('*');
    for (i=0; i<elements.length; i++) {
	if (elements[i].className == "popup"){
	    centerObj (elements[i]);
	}
    }
};
window.onscroll = window.onresize = centerAllPopups;

var popupClicked = 0;
var popupAnchor = null;
function makePopup (popupId, clicked) {
    if (clicked == 1 || popupClicked == 0) {
	hideAllPopups();
	var popupElement = document.getElementById (popupId);
	var popupAnchor = popupElement.parentNode;
	popupClicked = clicked;

//	popupAnchor.style.borderStyle = "solid";
//	popupAnchor.style.borderWidth = "thin";

	popupElement.style.display = "block";
	popupElement.onclick = function (e) { e.stopPropagation(); };
	centerObj (popupElement);
	
	document.onclick = clicked ? hideOnNextClickOutsidePopup : hideIfEventOutsidePopup;

	document.onmouseover = clicked ? null : hideOnNextMouseOutsidePopup;

	popupAnchor.removeAttribute ("title");  // prevent mouseover hint appearing after link already clicked, obscuring popup buttons
    }
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


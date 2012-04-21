// first define schoozNotify function
var schoozNotify = function() { };

// now do tests
var console_log = function(e) { puts(e); };
var dummy_log = function(e) { };
// console_log ("hi there, logging function seems to be working ");

// start interpreter
var intp = new BiwaScheme.Interpreter (console_log);

// function to run first action
var runFirstAction = function() {
    console_log ("Running first action");
    intp.evaluate ("(schooz:js-call-initial-action)", console_log);
};

// load Scheme files
var schemeFiles = ["schooz.scm", "js.scm", "api.scm", "rock.scm"];
var lastFunction = runFirstAction;
schemeFiles.reverse().forEach (function (theUrl) {
    // create local copies of variables, to force closures
    var theUrlLocal = theUrl;
    var lastFunctionLocal = lastFunction;
//    console_log ("creating xhr for " + theUrl);
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function() {
	if (xhr.readyState == 4) {
	    console_log ("Loaded " + xhr.responseText.length + " bytes from " + theUrlLocal);
//	    intp.evaluate (xhr.responseText, function(e) { console_log("evaluated Scheme: " + e); } );
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


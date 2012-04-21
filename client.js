// first define schoozNotify function
var schoozNotify = function() { };

// now do tests
var console_log = function(e) { puts(e); };
var dummy_log = function(e) { };
console_log ("hi there, logging function seems to be working ");

// load Scheme files
var intp = new BiwaScheme.Interpreter (dummy_log);
var schemeFiles = ["schooz.scm", "js.scm", "api.scm", "rock.scm"];
schemeFiles.forEach (function (the_url) {
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function() {
	if (xhr.readyState == 4) {
	    console_log ("loaded Scheme from " + the_url + " (" + xhr.responseText.length + " bytes)");
	    intp.evaluate (xhr.responseText, function(e) { console_log("evaluated Scheme: " + e); } );
	}
    };
    xhr.open("GET", the_url, true);
    xhr.send(null);
});

// run first action
intp.evaluate ("(schooz:js-call-initial-action)", console_log);

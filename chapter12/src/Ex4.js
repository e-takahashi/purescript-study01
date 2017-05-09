"use strict";

exports.stringifyImpl = function (x) {
    return JSON.stringify(x);
};

exports.readJSONImpl = function (json, onSuccess, onFailure) {
    console.log("**** readJSONImpl " + json);
    return function () {
	try {
	    var obj = JSON.parse(json);
	    console.log("*** onSuccess");
	    onSuccess(obj);
	} catch (e) {
	    console.log("*** onFailure "+ e);
	    onFailure(e.toString());
	}
    };
};

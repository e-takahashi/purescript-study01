"use strict";

exports.readJSONImpl = function (str, onSuccess, onFailure) {
    return function () {
	try {
	    var obj = JSON.parse(str);
	    onSuccess(obj);
	} catch (e) {
	    onFailure(e.toString());
	}
    };
};

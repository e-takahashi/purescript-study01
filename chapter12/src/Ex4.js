"use strict";

exports.stringifyImpl = function (x) {
    return JSON.stringify(x);
};

exports.readJSONImpl = function (json, onSuccess, onFailure) {
    try {
	var obj = JSON.parse(json);
	return onSuccess(obj);
    } catch (e) {
	return onFailure(e.toString());
    }
};

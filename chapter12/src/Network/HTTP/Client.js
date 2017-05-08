"use strict";

/*
exports.getImpl = function(uri, done, fail) {
    return function() {
        require('request')(uri, function(err, _, body) {
            if (err) {
                fail(err)();
            } else {
                done(body)();
            }
        });
    };
};
*/

function isHalf(c){
    return ((c >= 0x0 && c < 0x81) || (c == 0xf8f0) ||
	    (c >= 0xff61 && c < 0xffa0) || (c >= 0xf8f1 && c < 0xf8f4));
}

function removeMultiByte (src) {
    var dst = "";
    var i, n;
    for (i=0, n=src.length; i<n; i++) {
	var s = src.charAt(i);
	var c = src.charCodeAt(i);
	dst += (isHalf(c)? s: "*");
    }
    return dst;
}

exports.getImpl = function(uri, done, fail) {
    return function() {
        require('request')(uri, function(err, _, body) {
            if (err) {
                fail(removeMultiByte("error: " + err))();
            } else {
                done(removeMultiByte(body))();
            }
        });
    };
};

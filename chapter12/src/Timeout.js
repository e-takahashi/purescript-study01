"use strict";
/*
exports.setTimeoutImpl = function (ms, callback) {
    return function () {
        setTimeout(callback(), ms);
    };
};
*/
exports.setTimeoutImpl = function (ms, callback) {
    return function () {
        setTimeout(callback, ms);
    };
};

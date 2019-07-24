/// <reference lib="webworker" />
// @ts-check

"use strict";

/** @type {(global: unknown) => WorkerGlobalScope | null} */
exports._fromGlobalThis = function (global) {
	return (typeof WorkerGlobalScope !== 'undefined' && global instanceof WorkerGlobalScope) ? global : null;
}
// @ts-check

"use strict";

/**
 * @template T
 * @typedef {() => T} Effect
 */

/** @type {(request: Request) => (global: WindowOrWorkerGlobalScope) => Effect<Promise<Response>>} */
exports._fetch = function (request) {
	return function (global) {
		return function () {
			return global.fetch(request);
		}
	}
}
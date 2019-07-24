/// <reference lib="webworker" />
// @ts-check

"use strict";

/**
 * @template T
 * @typedef {() => T} Effect
 */

/** @type {(global: unknown) => ServiceWorkerGlobalScope | null} */
exports._fromGlobalThis = function (global) {
	return (typeof ServiceWorkerGlobalScope !== 'undefined' && global instanceof ServiceWorkerGlobalScope) ? global : null;
}

/** @type {(global: ServiceWorkerGlobalScope) => Effect<CacheStorage>} */
exports.caches = function (global) {
	return function () {
		return global.caches;
	}
}
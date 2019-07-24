/// <reference lib="webworker" />
// @ts-check

"use strict";

/**
 * @template T
 * @typedef {() => T} Effect
 */

/** @type {(event: Event) => FetchEvent | null} */
exports._fromEvent = function (event) {
	return (typeof FetchEvent !== 'undefined' && event instanceof FetchEvent) ? event : null;
}

/** @type {(event: FetchEvent) => Request} */
exports.request = function (event) {
	return event.request;
}

/** @type {(promise: Effect<Promise<Response>>) => (event: FetchEvent) => Effect<void>} */
exports._respondWith = function (promise) {
	return function (event) {
		return function () {
			event.respondWith(promise());
		}
	}
}
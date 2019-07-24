/// <reference lib="webworker" />
// @ts-check

"use strict";

/**
 * @template T
 * @typedef {() => T} Effect
 */

/** @type {(event: Event) => ExtendableEvent | null} */
exports._fromEvent = function (event) {
	return (typeof ExtendableEvent !== 'undefined' && event instanceof ExtendableEvent) ? event : null;
}

/** @type {(promise: Effect<Promise<void>>) => (event: ExtendableEvent) => Effect<void>} */
exports._waitUntil = function (promise) {
	return function (event) {
		return function () {
			event.waitUntil(promise());
		}
	}
}
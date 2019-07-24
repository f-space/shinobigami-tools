/// <reference lib="webworker" />
// @ts-check

"use strict";

/**
 * @template T
 * @typedef {() => T} Effect
 */

/** @type {(requests: string[]) => (cache: Cache) => Effect<Promise<void>>} */
exports._addAll = function (requests) {
	return function (cache) {
		return function () {
			return cache.addAll(requests);
		}
	}
}
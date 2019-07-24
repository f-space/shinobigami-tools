/// <reference lib="webworker" />
// @ts-check

"use strict";

/**
 * @template T
 * @typedef {() => T} Effect
 */

/** @type {(request: Request) => (caches: CacheStorage) => Effect<Promise<Response|undefined>>} */
exports._match = function (request) {
	return function (caches) {
		return function () {
			return caches.match(request);
		}
	}
}

/** @type {(cacheName: string) => (caches: CacheStorage) => Effect<Promise<Cache>>} */
exports._open = function (cacheName) {
	return function (caches) {
		return function () {
			return caches.open(cacheName);
		}
	}
}

/** @type {(cacheName: string) => (caches: CacheStorage) => Effect<Promise<boolean>>} */
exports._delete = function (cacheName) {
	return function (caches) {
		return function () {
			return caches.delete(cacheName);
		}
	}
}

/** @type {(caches: CacheStorage) => Effect<Promise<string[]>>} */
exports._keys = function (caches) {
	return function () {
		return caches.keys();
	}
}
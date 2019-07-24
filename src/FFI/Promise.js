// @ts-check

"use strict";

/**
 * @template T
 * @typedef {() => T} Effect
 */

/**
 * @template T
 * @callback Callback
 * @param {T} value
 * @returns {Effect<void>}
 */

/** @type {<T>(callback: (reject: Callback<Error>) => (resolve: Callback<T>) => Effect<void>) => Effect<Promise<T>>} */
exports._fromAff = function (callback) {
	return function () {
		return new Promise((resolve, reject) => {
			callback(reason => () => reject(reason))(value => () => resolve(value))();
		});
	}
}

/** @type {<T>(onRejected: Callback<Error>) => (onFulfilled: Callback<T>) => (promise: Effect<Promise>) => Effect<void>} */
exports._toAff = function (onRejected) {
	return function (onFulfilled) {
		return function (promise) {
			return function () {
				promise().then(value => onFulfilled(value)(), reason => onRejected(reason)());
			}
		}
	}
}
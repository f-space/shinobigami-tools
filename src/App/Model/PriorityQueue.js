// @ts-check

"use strict";

/**
 * @template T
 * @typedef Queue
 * @property {T[]} array
 * @property {boolean} dirty
 */

/**
 * @template T
 * @typedef {() => T} ST
 */

/**
 * @template T
 * @typedef {(a: T, b: T) => number} CompareFn
 */

/**
 * @template T
 * @type {ST<Queue<T>>}
 */
exports.empty = function () {
	return { array: [], dirty: false };
}

/** @type {<T>(queue: Queue<T>) => ST<boolean>} */
exports.null = function (queue) {
	return function () {
		return queue.array.length === 0;
	}
}

/** @type {<T>(value: T, queue: Queue<T>) => ST<void>} */
exports._push = function (value, queue) {
	return function () {
		queue.array.push(value);
		queue.dirty = true;
	}
}

/** @type {<T>(compareFn: CompareFn<T>, queue: Queue<T>) => ST<T>} */
exports._pop = function (compareFn, queue) {
	return function () {
		if (queue.array.length !== 0) {
			sortIfDirty(compareFn, queue);
			return queue.array.pop();
		}
		return null;
	}
}

/** @type {<T>(compareFn: CompareFn<T>, queue: Queue<T>) => ST<T>} */
exports._head = function (compareFn, queue) {
	return function () {
		if (queue.array.length !== 0) {
			sortIfDirty(compareFn, queue);
			return queue.array[queue.array.length - 1];
		}
		return null;
	}
}

/** @type {<T>(compareFn: CompareFn<T>, queue: Queue<T>) => void} */
function sortIfDirty(compareFn, queue) {
	if (queue.dirty) {
		queue.array.sort(compareFn);
		queue.dirty = false;
	}
}
// @ts-check

"use strict";

/**
 * @type {(x: number, y: number, document: Document) => () => Element}
 */
exports.elementFromPoint_ = function (x, y, document) {
	return function () {
		return Document.prototype.elementFromPoint.call(document, x, y);
	}
}

/**
 * @type {<T>(typeArg: string, eventInitDict?: CustomEventInit<T>) => () => CustomEvent<T>}
 */
exports.customEvent_ = function (typeArg, eventInitDict) {
	return function () {
		return new CustomEvent(typeArg, eventInitDict);
	}
}

/**
 * @type {<T>(event: CustomEvent<T>) => T}
 */
exports.detail = function (event) {
	return event.detail;
}
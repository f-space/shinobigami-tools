// @ts-check

"use strict";

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
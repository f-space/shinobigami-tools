// @ts-check

"use strict";

/** @type {(x: number, y: number, document: Document) => () => Element} */
exports._elementFromPoint = function (x, y, document) {
	return function () {
		return Document.prototype.elementFromPoint.call(document, x, y);
	}
}
"use strict";

export const newRange = new Range()

export const setStart = range => node => offset => () => range.setStart(node, offset)

export const setEnd = range => node => offset => () => range.setEnd(node, offset)

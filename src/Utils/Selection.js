"use strict";

export const anchorNode = selection => () => selection.anchorNode

export const anchorOffset = selection => () => selection.anchorOffset

export const addRange = selection => range => () => selection.addRange(range)

export const removeAllRanges = selection => () => selection.removeAllRanges()

export const selectAllChildren = selection => node => () => selection.selectAllChildren(node)

export const collapseToEnd = selection => () => selection.collapseToEnd()

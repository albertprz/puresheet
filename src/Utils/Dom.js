"use strict";

export const innerText = el => () => el.innerText

export const setInnerHTML = el => html => () => el.innerHTML = html

export const getSelection = window => () => window.getSelection()

export const refEquals = x => y => x == y

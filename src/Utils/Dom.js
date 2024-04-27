"use strict";

export const setInnerHTML_ = el => html => () => el.innerHTML = html

export const scrollIntoView = el => () =>
    el.scrollIntoView ({ behaviour: "smooth",
                         block: "center",
                         inline: "center" });

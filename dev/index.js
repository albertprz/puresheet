import fs from "fs";
import path from "path";


const prelude = fs.readFileSync(path.join(__dirname, "../lib/Prelude.pursh"), "utf8");

const matrix = fs.readFileSync(path.join(__dirname, "../lib/Matrix.pursh"), "utf8");

const array = fs.readFileSync(path.join(__dirname, "../lib/Array.pursh"), "utf8");

window.loadedModules = [prelude, matrix, array]

require("../output/Main/index.js").main();

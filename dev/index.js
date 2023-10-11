import fs from "fs";
import path from "path";

window.prelude = fs.readFileSync(path.join(__dirname, "../lib/Prelude.ps"), "utf8");

require("../output/Main/index.js").main();

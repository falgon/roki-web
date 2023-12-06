const fs = require("fs");
const katex = require("katex");

const input = fs.readFileSync(process.stdin.fd, "utf-8");
const displayMode = process.argv.includes("displayMode");
const mathjaxOpt = {
    displayMode,
    trust: true,
    colorIsTextColor: true
};

process.stdout.write(katex.renderToString(input, mathjaxOpt));

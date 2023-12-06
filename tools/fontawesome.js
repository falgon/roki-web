#!/usr/bin/env node

const { icon } = require("@fortawesome/fontawesome-svg-core");

const pkgs = [
    require("@fortawesome/free-brands-svg-icons"),
    require("@fortawesome/free-solid-svg-icons")
];

let obj = {};
for (const p of pkgs) {
    obj[p.prefix] = {};
    for (const i of Object.values(p[p.prefix])) {
        obj[p.prefix][i.iconName] = icon(i).abstract[0];
    }
}
process.stdout.write(JSON.stringify(obj));

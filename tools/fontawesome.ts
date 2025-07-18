#!/usr/bin/env tsx

import { icon } from "@fortawesome/fontawesome-svg-core";

const pkgs = [
    require("@fortawesome/free-brands-svg-icons"),
    require("@fortawesome/free-solid-svg-icons")
];

interface IconObject {
    [prefix: string]: {
        [iconName: string]: any;
    };
}

const obj: IconObject = {};
for (const p of pkgs) {
    obj[p.prefix] = {};
    for (const i of Object.values(p[p.prefix])) {
        obj[p.prefix][(i as any).iconName] = icon(i as any).abstract[0];
    }
}
process.stdout.write(JSON.stringify(obj)); 
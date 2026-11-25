#!/usr/bin/env tsx

import { type AbstractElement, icon } from "@fortawesome/fontawesome-svg-core";
import { fab } from "@fortawesome/free-brands-svg-icons";
import { fas } from "@fortawesome/free-solid-svg-icons";

const icons: Record<string, Record<string, AbstractElement>> = {};
for (const [prefix, pack] of Object.entries({ fab, fas })) {
    const names: Record<string, AbstractElement> = {};
    for (const definition of new Set(Object.values(pack))) {
        const [svg] = icon(definition).abstract;
        if (!svg) continue;

        names[definition.iconName] = svg;
        for (const alias of definition.icon[2]) {
            if (typeof alias === "string") {
                names[alias] = svg;
            }
        }
    }
    icons[prefix] = names;
}
process.stdout.write(JSON.stringify(icons));

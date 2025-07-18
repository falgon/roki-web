#!/usr/bin/env tsx

import * as ts from 'typescript';

let input = '';
process.stdin.setEncoding('utf8');
process.stdin.on('data', (chunk: string) => input += chunk);
process.stdin.on('end', () => {
  const result = ts.transpileModule(input, {
    compilerOptions: {
      target: ts.ScriptTarget.ES2020,
      module: ts.ModuleKind.CommonJS,
      lib: ['ES2020', 'DOM'],
      strict: true,
      skipLibCheck: true,
      noEmitOnError: true,
      sourceMap: false,
      removeComments: true,
      declaration: false,
      esModuleInterop: true
    }
  });
  console.log(result.outputText);
}); 
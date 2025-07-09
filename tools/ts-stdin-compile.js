#!/usr/bin/env node

const ts = require('typescript');

let input = '';
process.stdin.setEncoding('utf8');
process.stdin.on('data', chunk => input += chunk);
process.stdin.on('end', () => {
  const result = ts.transpileModule(input, {
    compilerOptions: {
      target: ts.ScriptTarget.ES6,
      module: ts.ModuleKind.CommonJS,
      lib: ['ES6', 'DOM'],
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
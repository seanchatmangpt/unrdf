import fs from 'fs';
import path from 'path';

/**
 * Parses a file for TODO/FIXME markers and returns an RDF Turtle representation.
 */
function extractStubs(filePath) {
  const content = fs.readFileSync(filePath, 'utf-8');
  const lines = content.split('\n');
  const stubs = [];
  
  lines.forEach((line, index) => {
    const match = line.match(/\/\/\s*(TODO|FIXME|STUB|IMPLEMENT):\s*(.*)/);
    if (match) {
      const type = match[1];
      const description = match[2];
      stubs.push({
        id: `stub_${Date.now()}_${index}`,
        file: filePath,
        line: index + 1,
        type,
        description
      });
    }
  });
  return stubs;
}

const prototypePath = path.join(process.cwd(), 'research/prototypes');
const files = fs.readdirSync(prototypePath);
let allStubs = [];

files.forEach(file => {
  if (file.endsWith('.mjs') || file.endsWith('.js')) {
    allStubs = [...allStubs, ...extractStubs(path.join(prototypePath, file))];
  }
});

const turtle = `
@prefix ex: <http://example.org/stub#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

${allStubs.map(s => `
ex:${s.id} a ex:Stub ;
    ex:type "${s.type}" ;
    ex:description "${s.description.replace(/"/g, '\\"')}" ;
    ex:file "${s.file}" ;
    ex:line ${s.line} .
`).join('\n')}
`;

fs.writeFileSync('stubs.ttl', turtle);
console.log('Generated stubs.ttl from prototype research files.');

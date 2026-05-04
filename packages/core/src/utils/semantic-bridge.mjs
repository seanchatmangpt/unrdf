import { execSync } from 'child_process';
import path from 'path';
import fs from 'fs';
import os from 'os';

/**
 * Executes a SPARQL query against the Open Ontologies engine using the CLI.
 * 
 * @param {string} query - The SPARQL query to run.
 * @param {Object} options - Options for the execution.
 * @param {string[]} [options.ontologyFiles] - Paths to .ttl or .nt files to load.
 * @param {string[]} [options.rawTriples] - Arrays of raw N-Triples or Turtle strings to load.
 * @returns {Object} The JSON parsed result.
 */
export function executeSemanticQuery(query, { ontologyFiles = [], rawTriples = [] } = {}) {
  // Use environment variable or default relative path
  const ooPath = process.env.OPEN_ONTOLOGIES_PATH || path.join(process.cwd(), 'open-ontologies/target/debug/open-ontologies');
  
  if (!fs.existsSync(ooPath)) {
     // Try one level up if we are in a package
     const alternatePath = path.join(process.cwd(), '../open-ontologies/target/debug/open-ontologies');
     if (!fs.existsSync(alternatePath)) {
        // Try two levels up for deeper packages
        const alternatePath2 = path.join(process.cwd(), '../../open-ontologies/target/debug/open-ontologies');
        if (!fs.existsSync(alternatePath2)) {
            throw new Error(`Open Ontologies CLI not found at ${ooPath}`);
        }
        return _execute(alternatePath2, query, { ontologyFiles, rawTriples });
     }
     return _execute(alternatePath, query, { ontologyFiles, rawTriples });
  }

  return _execute(ooPath, query, { ontologyFiles, rawTriples });
}

function _execute(ooPath, query, { ontologyFiles, rawTriples }) {
  const tmpDir = os.tmpdir();
  const batchFile = path.join(tmpDir, `oo-batch-${Date.now()}.txt`);
  const tempFiles = [];
  
  let batchCommands = '';
  
  try {
    for (const file of ontologyFiles) {
      if (fs.existsSync(file)) {
        batchCommands += `load ${path.resolve(file)}\n`;
      }
    }
    
    for (let i = 0; i < rawTriples.length; i++) {
      const rawFile = path.join(tmpDir, `oo-raw-${Date.now()}-${i}.nt`);
      fs.writeFileSync(rawFile, rawTriples[i]);
      tempFiles.push(rawFile);
      batchCommands += `load ${rawFile}\n`;
    }
    
    batchCommands += `reason rdfs\n`;
    
    const sanitizedQuery = query.replace(/\n/g, ' ').replace(/"/g, '\\"');
    batchCommands += `query "${sanitizedQuery}"\n`;
    
    fs.writeFileSync(batchFile, batchCommands);
    tempFiles.push(batchFile);
    
    const output = execSync(`${ooPath} batch ${batchFile}`, { encoding: 'utf-8', stdio: ['pipe', 'pipe', 'pipe'] });
    
    const lines = output.trim().split('\n');
    const queryResultLine = lines[lines.length - 1];
    
    const parsed = JSON.parse(queryResultLine);
    
    if (parsed.result && parsed.result.error) {
       throw new Error(parsed.result.error);
    }
    
    return parsed.result;
  } catch (err) {
    throw err;
  } finally {
    for (const file of tempFiles) {
      if (fs.existsSync(file)) fs.unlinkSync(file);
    }
  }
}

import { parseTurtle, select } from 'unrdf'
import { readFile, writeFile, mkdir } from 'fs/promises'
import { dirname } from 'path'
import { traced } from '../../../telemetry/tracer.mjs'

/**
 * Generate an artifact from a Turtle graph and a simple Markdown template.
 * @param {string} graphPath Path to Turtle file
 * @param {string} templatePath (ignored in this demo)
 * @param {string} outPath Output file path
 */
export const generate = traced('generate', async (graphPath, templatePath, outPath) => {
  // Read and parse the Turtle graph
  const ttl = await readFile(graphPath, 'utf8')
  const store = parseTurtle(ttl)
  const data = await select(store, 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }')
  // Simple Markdown report
  let result = '# Knowledge Graph Export\n\n'
  result += 'Graph Data:\n'
  result += data.map(d => `- Subject: ${d.s}, Predicate: ${d.p}, Object: ${d.o}`).join('\n')
  // Ensure output directory exists
  const outDir = dirname(outPath)
  await mkdir(outDir, { recursive: true })
  await writeFile(outPath, result, 'utf8')
})
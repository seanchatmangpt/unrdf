#!/usr/bin/env node
import { program } from 'commander'
import { startTelemetry, shutdownTelemetry } from '../../../telemetry/otel-setup.mjs'
import { generate } from './index.mjs'

await startTelemetry()

program
  .name('kgen')
  .description('Knowledge artifact generator CLI')
  .command('render <graph> <template> <out>')
  .description('Generate artifact from graph and template')
  .action(async (graph, template, out) => {
    let code = 0
    try {
      await generate(graph, template, out)
      console.log(`Generated ${out}`)
    } catch (err) {
      console.error(err)
      code = 1
    }
    try {
      await shutdownTelemetry()
    } catch {}
    process.exit(code)
  })

program.parseAsync(process.argv)
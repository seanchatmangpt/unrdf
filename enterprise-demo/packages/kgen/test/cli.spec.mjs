import { given, when, then } from 'citty-test-utils'
import { execa } from 'execa'
import path from 'path'
import fs from 'fs-extra'

given('example graph and template', ctx => {
  ctx.graph = path.resolve('examples/demo/graph.ttl')
  ctx.template = path.resolve('examples/demo/template.hbs')
  ctx.out = path.resolve('examples/demo/out.md')
  fs.removeSync(ctx.out)
})

when('we run kgen render', async ctx => {
  await execa('node', ['packages/kgen/src/cli.mjs', 'render', ctx.graph, ctx.template, ctx.out])
})

then('the output file exists', ctx => {
  expect(fs.existsSync(ctx.out)).toBe(true)
  const content = fs.readFileSync(ctx.out, 'utf8')
  expect(content).toContain('Graph Data:')
})
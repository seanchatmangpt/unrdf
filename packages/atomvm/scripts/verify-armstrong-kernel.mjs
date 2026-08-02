#!/usr/bin/env node
import { createHash } from 'node:crypto';
import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';

const expected = Object.freeze([
  'isolated_state',
  'sender_order',
  'selective_receive',
  'crash_isolation',
  'links_and_trap_exit',
  'monitors_down',
  'restart_after_failure',
  'tail_recursive_server',
]);

const [logPath, outputArg] = process.argv.slice(2);
if (!logPath) throw new Error('usage: verify-armstrong-kernel.mjs <atomvm.log> [receipt.json]');
const log = await readFile(resolve(logPath), 'utf8');
const observed = [...log.matchAll(/\{armstrong_kernel,([a-z_]+),ok\}/g)].map(match => match[1]);
const missing = expected.filter(marker => !observed.includes(marker));
const unexpected = observed.filter(marker => !expected.includes(marker));
const duplicates = observed.filter((marker, index) => observed.indexOf(marker) !== index);
const finalMarker = log.includes(`{atomvm_armstrong_kernel_alive,${expected.length}}`);
const returnOk = log.includes('Return value: ok');
const body = {
  schema: 'urn:unrdf:atomvm:armstrong-kernel-receipt:v1',
  expected,
  observed,
  missing,
  unexpected,
  duplicates,
  finalMarker,
  returnOk,
  status: missing.length === 0 && unexpected.length === 0 && duplicates.length === 0 && finalMarker && returnOk
    ? 'ALIVE'
    : 'BLOCKED',
};
body.receiptDigest = createHash('sha256').update(JSON.stringify(body)).digest('hex');
const output = resolve(outputArg ?? '.build/receipts/armstrong-kernel-receipt.json');
await mkdir(dirname(output), { recursive: true });
await writeFile(output, `${JSON.stringify(body, null, 2)}\n`);
console.log(JSON.stringify({ status: body.status, observed: observed.length, receiptDigest: body.receiptDigest }));
if (body.status !== 'ALIVE') process.exit(1);

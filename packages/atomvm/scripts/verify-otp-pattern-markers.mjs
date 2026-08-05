#!/usr/bin/env node
import { createHash } from 'node:crypto';
import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';

const expected = [
  'immutable_messages',
  'sealed_message_protocols',
  'state_as_value',
  'result_railway',
  'domain_types_over_primitives',
  'pure_state_handlers',
  'compose_by_purpose',
  'railway_composition',
  'test_without_framework',
  'skinny_left_margin',
  'process_as_boundary',
  'tell_dont_block',
  'ask_with_timeout',
  'stable_references',
  'named_processes',
  'trap_exits',
  'let_it_crash',
  'supervision_trees',
  'restart_intensity',
  'supervised_startup',
  'links_shared_fate',
  'monitors_observation',
  'retry_fresh_state',
  'state_machines',
  'event_broadcasting',
  'timed_messages',
  'fan_out_fail_fast',
  'process_introspection',
  'assemble_application',
  'test_the_boundary',
];

const logPath = resolve(process.argv[2] ?? '.build/otp-patterns/atomvm.log');
const receiptPath = resolve(process.argv[3] ?? '.build/receipts/otp-patterns-receipt.json');
const text = await readFile(logPath, 'utf8');
const observed = [...text.matchAll(/\{otp_pattern,([^,}]+),ok\}/g)].map(match => match[1]);
const unique = [...new Set(observed)];
const missing = expected.filter(name => !unique.includes(name));
const unexpected = unique.filter(name => !expected.includes(name));
const duplicates = unique.filter(name => observed.filter(value => value === name).length !== 1);
const finalMarker = text.includes('{atomvm_otp_patterns_alive,30}');
const returnedOk = text.includes('Return value: ok');
const status = missing.length === 0 && unexpected.length === 0 && duplicates.length === 0 &&
  observed.length === expected.length && finalMarker && returnedOk ? 'ALIVE' : 'BLOCKED';

const body = {
  schema: 'urn:unrdf:atomvm:otp-patterns-receipt:v1',
  methodology: 'Chicago/Detroit TDD',
  expected,
  observed,
  missing,
  unexpected,
  duplicates,
  finalMarker,
  returnedOk,
  status,
  logDigest: createHash('sha256').update(text).digest('hex'),
};
body.receiptDigest = createHash('sha256').update(JSON.stringify(body)).digest('hex');
await mkdir(dirname(receiptPath), { recursive: true });
await writeFile(receiptPath, `${JSON.stringify(body, null, 2)}\n`);
console.log(JSON.stringify(body));
if (status !== 'ALIVE') process.exit(1);

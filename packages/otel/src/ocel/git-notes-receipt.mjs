import { createHash } from 'node:crypto';
import { createGitSwarmOcel, validateOcel2Document } from './git-swarm.mjs';
export const GIT_SWARM_RECEIPT_SCHEMA='unrdf.git-swarm.ocel.v1';
const canon=v=>Array.isArray(v)?v.map(canon):(v&&typeof v==='object'?Object.fromEntries(Object.keys(v).sort().map(k=>[k,canon(v[k])])):v);
export function serializeGitSwarmReceipt(document){const v=validateOcel2Document(document);if(!v.valid)throw new TypeError(v.errors.join(';'));const s=JSON.stringify(canon(document));return{schema:GIT_SWARM_RECEIPT_SCHEMA,digest:'sha256:'+createHash('sha256').update(s).digest('hex'),document:JSON.parse(s)}}
export function prepareGitSwarmReceipt(input){return serializeGitSwarmReceipt(createGitSwarmOcel(input))}
export function verifyGitSwarmReceipt(r){if(r?.schema!==GIT_SWARM_RECEIPT_SCHEMA)return{valid:false,errors:['schema mismatch']};const v=validateOcel2Document(r.document);if(!v.valid)return v;return r.digest===serializeGitSwarmReceipt(r.document).digest?{valid:true,errors:[]}:{valid:false,errors:['digest mismatch']}}
export async function persistGitSwarmReceipt(input,writeReceipt,{ref='refs/notes/gitvan/results'}={}){if(typeof writeReceipt!=='function')throw new TypeError('writeReceipt required');const r=prepareGitSwarmReceipt(input);await writeReceipt(r,{ref,sha:input.commit});return r}
export function replayGitSwarmReceipt(r){const v=verifyGitSwarmReceipt(r);if(!v.valid)throw new TypeError(v.errors.join(';'));return r.document}

import {describe,it,expect} from 'vitest';
import {admitSwarmIdentity,bindExactSwarmSubject} from '../../src/ocel/swarm-contracts.mjs';
const good={repository:'seanchatmangpt/unrdf',branch:'feat/x',commit:'a'.repeat(40),baseCommit:'b'.repeat(40),task:'T125',tool:'github.create_tree',sequence:125};
describe('swarm exact identity',()=>{it('admits exact identity',()=>expect(admitSwarmIdentity(good)).toEqual({admitted:true,refusals:[]}));it('refuses missing tool',()=>expect(admitSwarmIdentity({...good,tool:''}).admitted).toBe(false));it('refuses abbreviated sha',()=>expect(admitSwarmIdentity({...good,commit:'abc123'}).refusals[0].code).toBe('commit_not_full_sha'));it('binds deterministic subject',()=>expect(bindExactSwarmSubject(good)).toContain('seanchatmangpt/unrdf@'));});

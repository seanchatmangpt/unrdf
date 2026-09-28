/** Exact-subject admission registry for Git coding-swarm receipts. */
export const SWARM_CONTRACT_DIMENSIONS = Object.freeze([
  'repository','branch','commit','task','tool','surface','authority','provenance','sequence','failure','receipt','replay','consumer','migration'
]);
export function admitSwarmIdentity(input) {
  const refusals=[];
  for (const key of ['repository','branch','commit','task','tool']) if (!input?.[key]) refusals.push({dimension:key,code:`${key}_identity_missing`});
  if (input?.commit && !/^[0-9a-f]{40}$/i.test(input.commit)) refusals.push({dimension:'commit',code:'commit_not_full_sha'});
  if (input?.baseCommit && !/^[0-9a-f]{40}$/i.test(input.baseCommit)) refusals.push({dimension:'commit',code:'base_commit_not_full_sha'});
  if (input?.sequence !== undefined && (!Number.isInteger(input.sequence)||input.sequence<1)) refusals.push({dimension:'sequence',code:'invalid_sequence'});
  return {admitted:refusals.length===0,refusals};
}
export function bindExactSwarmSubject({repository,branch,commit,task,tool}) { return `${repository}@${commit}#${branch}:${task}:${tool}`; }

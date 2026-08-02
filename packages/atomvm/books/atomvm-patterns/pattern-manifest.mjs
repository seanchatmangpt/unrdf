export const SOURCE = Object.freeze({
  repository: 'seanchatmangpt/jotp',
  commit: '41d2716af7b7bec272b2df5e36d0781ddb7a2df5',
  summaryPath: 'books/jotp-patterns/src/SUMMARY.md',
});

const definitions = [
  ['part-1-shape-your-data/immutable-messages.md', 'immutable_messages'],
  ['part-1-shape-your-data/sealed-message-protocols.md', 'sealed_message_protocols'],
  ['part-1-shape-your-data/state-as-value.md', 'state_as_value'],
  ['part-1-shape-your-data/result-railway.md', 'result_railway'],
  ['part-1-shape-your-data/domain-types.md', 'domain_types_over_primitives'],
  ['part-2-functional-core/pure-state-handlers.md', 'pure_state_handlers'],
  ['part-2-functional-core/compose-by-purpose.md', 'compose_by_purpose'],
  ['part-2-functional-core/railway-composition.md', 'railway_composition'],
  ['part-2-functional-core/test-without-framework.md', 'test_without_framework'],
  ['part-2-functional-core/skinny-left-margin.md', 'skinny_left_margin'],
  ['part-3-process-boundaries/process-as-boundary.md', 'process_as_boundary'],
  ['part-3-process-boundaries/tell-dont-block.md', 'tell_dont_block'],
  ['part-3-process-boundaries/ask-with-timeout.md', 'ask_with_timeout'],
  ['part-3-process-boundaries/stable-references.md', 'stable_references'],
  ['part-3-process-boundaries/named-processes.md', 'named_processes'],
  ['part-3-process-boundaries/trap-exits.md', 'trap_exits'],
  ['part-4-lifecycle/let-it-crash.md', 'let_it_crash'],
  ['part-4-lifecycle/supervision-trees.md', 'supervision_trees'],
  ['part-4-lifecycle/restart-intensity.md', 'restart_intensity'],
  ['part-4-lifecycle/supervised-startup.md', 'supervised_startup'],
  ['part-4-lifecycle/links-shared-fate.md', 'links_shared_fate'],
  ['part-4-lifecycle/monitors-observation.md', 'monitors_observation'],
  ['part-4-lifecycle/retry-fresh-state.md', 'retry_fresh_state'],
  ['part-5-workers-assembly/state-machines.md', 'state_machines'],
  ['part-5-workers-assembly/event-broadcasting.md', 'event_broadcasting'],
  ['part-5-workers-assembly/timed-messages.md', 'timed_messages'],
  ['part-5-workers-assembly/fan-out-fail-fast.md', 'fan_out_fail_fast'],
  ['part-5-workers-assembly/process-introspection.md', 'process_introspection'],
  ['part-5-workers-assembly/assemble-application.md', 'assemble_application'],
  ['part-5-workers-assembly/test-the-boundary.md', 'test_the_boundary'],
];

export const PATTERNS = Object.freeze(definitions.map(([chapter, atomvmMarker], index) => {
  const id = index + 1;
  const part = id <= 5 ? 1 : id <= 10 ? 2 : id <= 16 ? 3 : id <= 23 ? 4 : 5;
  const testFile = id <= 15
    ? 'otp-patterns-data-chicago.test.mjs'
    : id <= 23
      ? 'otp-patterns-lifecycle-chicago.test.mjs'
      : 'otp-patterns-workers-chicago.test.mjs';
  return Object.freeze({
    id,
    sourcePath: `books/jotp-patterns/src/${chapter}`,
    targetPath: `packages/atomvm/books/atomvm-patterns/src/${chapter}`,
    examplePath: `packages/atomvm/books/atomvm-patterns/examples/part-${part}.mjs`,
    testPath: `packages/atomvm/test/${testFile}`,
    testName: `Pattern ${id}`,
    atomvmMarker,
  });
}));

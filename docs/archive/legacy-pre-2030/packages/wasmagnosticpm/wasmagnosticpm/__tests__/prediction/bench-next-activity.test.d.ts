/**
 * Next Activity Prediction Benchmarks
 * Perspective: "What happens next?" — Van der Aalst
 *
 * Algorithms:
 *   predict_next_activity  — basic n-gram prediction (returns [{activity, probability}])
 *   score_trace_likelihood — n-gram log-probability of a full trace (plain float)
 *   predict_next_k         — top-k with confidence + entropy
 *   predict_beam_paths     — beam search over future continuations
 *
 * Each test is self-contained: loads its own log and builds its own handles.
 * setup.ts calls clear_all_objects() before each test, so handles cannot be
 * shared across tests via beforeAll.
 */
export {};
//# sourceMappingURL=bench-next-activity.test.d.ts.map

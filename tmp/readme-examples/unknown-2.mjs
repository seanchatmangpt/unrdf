import { createAdmissionEngine, wouldAdmit } from './src/admission/admission-engine.mjs';

// Create engine with config
const engine = createAdmissionEngine({ maxTriples: 10000 });

// Check if a delta capsule would be admitted
const admitted = await wouldAdmit(capsule, { maxTriples: 10000 });
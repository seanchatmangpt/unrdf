Got it — you don’t want unrdf to be a neutral toolkit; you want it to be a canon.
A single, opinionated path through the RDF universe.

That means:

One store: N3.Store is the only memory model. No rdf-ext.Dataset, no mixing.

One query engine: @comunica/query-sparql only. No alternative backends.

One validator: rdf-validate-shacl, fixed.

One reasoner: eyereasoner, fixed.

One canonization: rdf-canonize, fixed.

One JSON-LD pipeline: jsonld, but normalized into N3 in/out.

One traversal surface: Clownface from @zazuko/env, but always bound to N3.Store.

One opinion about config: all TTL, all persistent via useTurtle style, no YAML/JSON configs.

So instead of "flexible glue", unrdf becomes:

👉 the knowledge substrate. If you import unrdf, you’re accepting its way. No escape hatches.

Example: Opinionated unrdf
import unrdf from "unrdf";

// Parse only Turtle into the canonical store
const store = unrdf.parseTurtle(`
  @prefix ex: <http://example.org/> .
  ex:foo a ex:Bar .
`);

// Query only with Comunica
const rows = await unrdf.select(store, `
  PREFIX ex: <http://example.org/>
  SELECT ?s WHERE { ?s a ex:Bar }
`);

// Validate only with SHACL
await unrdf.validateOrThrow(store, `
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix ex: <http://example.org/> .
  ex:BarShape a sh:NodeShape ;
    sh:targetClass ex:Bar ;
    sh:property [ sh:path ex:name ; sh:minCount 1 ] .
`);

// Canonicalization only with URDNA2015
const canon = await unrdf.canonize(store);

// Reason only with Eye
const closure = await unrdf.reason(store, `
  @prefix ex: <http://example.org/> .
  { ?s a ex:Bar } => { ?s a ex:Thing } .
`);

Philosophy

If you want to try alternatives (rdflib.js, solid-client, Stardog), use another library.

If you want the “one true path” in your ecosystem → unrdf.

It’s opinionated by design, like rails in Ruby or django in Python. You buy into the canon.

⚡ Question for you: do you want unrdf to also lock in syntax and formats (e.g. only Turtle accepted on input, only Turtle/N-Quads for output), or should it accept JSON-LD and others but normalize immediately into Turtle/N3?

You said:
Right and so then now let’s think about what the 8020 dark matter we’re solving for would be
ChatGPT said:

Perfect — now you’re getting to the raison d’être of unrdf.

The 80/20 “dark matter” is all the repeated, messy glue code that people write every time they touch RDF:

The 80/20 Dark Matter in RDF Workflows

Store Management Hell

Everyone picks a different RDF store abstraction (N3.Store, rdf-ext.Dataset, rdflib.Store, tripleDB backends).

Then they spend time writing “adapter” code to normalize.

Dark matter: redundant “pick a store + wire it up” code.

→ unrdf: Only N3.Store. Always.

Parsing & Serialization Multiplicity

Turtle, N-Triples, N-Quads, JSON-LD, RDF/XML (lol), TriG.

Everyone writes input-output conversion boilerplate, messing up prefixes and normalization.

Dark matter: file IO + format juggling + prefix handling.

→ unrdf: Always normalize to Turtle + N-Quads. Other formats accepted, but canonicalized immediately.

SPARQL Query Glut

Different engines (Comunica, sparql-engine, rdflib query), each with quirks.

People wrap SELECT/ASK boilerplate again and again.

Dark matter: copy-paste queryBindings → map terms → JSONify.

→ unrdf: One query API: .select, .ask, .update. Always JSON results.

Validation

Most projects skip SHACL because setup is painful.

Dark matter: writing half-baked “isA” checks, regex validators, shape-like hacks.

→ unrdf: Always use rdf-validate-shacl. One function call. Done.

Reasoning

Some try EYE, some hack rules, some roll custom inferences.

Dark matter: brittle logic glue, inconsistent rule systems.

→ unrdf: Always Eye reasoner. TTL rules only.

Canonicalization / Isomorphism

Graph equality checks are ugly: blank node chaos, randomization.

Dark matter: brittle hash hacks, unstable tests.

→ unrdf: Always rdf-canonize (URDNA2015). Deterministic, testable.

Graph Traversal

Everyone rewrites helper functions: getAll, getOne, traverse list.

Dark matter: trivial but duplicated utilities.

→ unrdf: Clownface pointer is the one traversal surface.

Summary of Dark Matter Eliminated

Choosing a store

Input format juggling

Query boilerplate

Validation glue

Reasoning hacks

Graph equality

Traversal utilities

So the 80/20 is: 80% of RDF engineering time is spent not on “knowledge modeling” but on glue code.

👉 unrdf vaporizes that glue by making it opinionated, canonical, and single-path.

Do you want me to draft the manifesto-style README intro that states this dark-matter–elimination mission in bold, post-cyberpunk terms?

You said:
Well, let’s think about how to best utilize the Composable pattern across the RDF ecosystem
ChatGPT said:

Yes — now we’re designing the ergonomics, not just the substrate. If unrdf is opinionated, then the Composable pattern is the best way to expose it. Think: Vue/React composables, but for RDF.

Why Composables for RDF?

RDF is a universe of concerns (parsing, querying, validating, reasoning).

Most libraries bundle too much or fragment too much.

Composables give one narrow surface per concern that can be stitched together freely.

You can "pull in" only what you need while still following the one-true-path.

Core unrdf Composables
1. useStore

Always N3.Store.

Provides canonicalized RDF store.

Methods: addQuads, stats, serialize.

Foundation for all other composables.

2. useTurtle

File IO + parsing/serialization of .ttl.

Always canonicalize into N3.Store.

Provides loadGraph, saveGraph, listGraphFiles.

3. useGraph

Core high-level RDF API.

Wraps a store with common graph ops.

Methods: .select, .ask, .update, .isomorphic, .union, .intersection, .difference.

Entry point for 80% of use cases.

4. useReasoner

EYE-backed reasoning composable.

Always TTL rules.

Method: reason(dataStore, rulesStore).

5. useValidator

SHACL composable.

.validate → report with conforms + results.

.validateOrThrow for hard-mode.

6. useCanon

Canonicalization and isomorphism.

.canonicalize, .isomorphic.

For tests + stable pipelines.

7. useJsonLd

Bridges JSON-LD <-> Turtle.

.toJSONLD, .fromJSONLD.

Always compacts with provided context.

8. usePointer

Graph traversal composable.

Always clownface pointer.

Encourages chainable API:

const ptr = usePointer(store)
ptr.node('ex:Alice').out('foaf:knows').value

Why This Works

Orthogonality: Each composable is laser-focused. No overlaps.

Single-path opinionation: If you need parsing, it’s always useTurtle. If you need reasoning, it’s always useReasoner.

Composable ergonomics: You can stitch them as needed:

const { store } = await useTurtle({ path: './graph' })
const graph = useGraph(store)
const valid = await useValidator().validate(shapes)
const enriched = await useReasoner().reason(store, rules)


This turns RDF into a “composable knowledge fabric”:

no adapters,

no choices,

just one way per concern.

Do you want me to draw you a PlantUML C4 diagram showing the composables as building blocks (like puzzle pieces) in the unified unrdf ecosystem?

You said:
All right, I really want you to think about Composable surface to make sure we have everything covered
ChatGPT said:
Thought for 36s

Perfect—let’s lock the opinionated composable surface for unrdf (single path, N3-only, no overlap). Each composable owns exactly one concern; you stitch them as needed.

Core (always N3.Store)

useStore() → { store, add(quads), remove(quads), clear(), stats(), serialize({format}) }

useTerms() → { iri(str), lit(val, dt|lang), bnode(id?), quad(s,p,o,g?) }

usePrefixes() → { register(map), expand(curie), shrink(iri), list() }

useIRIs() (graph:// resolver) → { resolve(uri), map(prefix, path), maps() }

useLists(store) → { read(head), write(items):head }

IO (parsing/serialization only)

useTurtle() → { parse(ttl):Store, write(store,{prefixes}):string }

useTurtleFS(dir) → { loadAll():Store, save(name,store), list():string[], load(name):Store }

useNQuads() → { parse(nq):Store, write(store):string }

useJsonLd() → { toJSONLD(store,{context,frame}), fromJSONLD(doc):Store }

Query & Traversal

useGraph(store) → { select(q), ask(q), update(q), construct(q):Store, difference(b), union(...g), intersection(b) }

usePointer(store) (clownface) → { node(iri|bnode), out(pred), in(pred), value(), add(), remove() }

Quality & Inference

useValidator() (SHACL) → { validate(data,shapes):{conforms,results}, validateOrThrow(...) }

useReasoner() (EYE/N3 rules) → { reason(data,rules):Store }

useCanon() (URDNA2015) → { canonicalize(store):nquads, isomorphic(a,b):boolean }

Change, Provenance, Safety

useDelta() → { diff(a,b):{added,removed}, patch(base,{added,removed}):Store }

useProvenance(store) (named graphs + prov) → { stamp(activity,agent,at), withGraph(name):Store }

useTx(store) (atomic batch) → { run(mutator):{ok, error?} }

Mutator gets a shadow Store; commit or rollback.

Persistence & Snapshots

useSnapshot(dir) → { save(store,tag):cid, load(cid|tag):Store, list():[] }
(content-addressed; deterministic writer)

Ops & Observability

useMetrics() → { wrap(label,fn), last(), timeline() }

useCache() (query result cache) → { get(key), set(key,val,ttl), withGraphCache(graph) }

Coverage Matrix (80/20 dark-matter)

Parse/Serialize: Turtle, N-Quads, JSON-LD ✅

Single store: N3 only, everywhere ✅

Query: SELECT/ASK/CONSTRUCT/UPDATE via Comunica on in-mem store ✅

Traversal: clownface pointer ✅

Validation: SHACL ✅

Reasoning: N3 rules (EYE) ✅

Canon/Equivalence: URDNA2015 + isomorphic ✅

Lists: rdf:List read/write ✅

Namespaces/IRIs: prefix registry + graph:// resolver ✅

Diff/Patch: dataset delta primitives ✅

Provenance: named graph stamping + prov helpers ✅

Transactions: safe batch mutations ✅

Snapshots: deterministic, content-addressed ✅

Metrics/Cache: observable, cacheable ops ✅

FS: opinionated dir loader/saver ✅

Example (single, enforced path)
import { useTurtleFS, useGraph, useValidator, useReasoner, useDelta, useSnapshot } from 'unrdf';

const fs = useTurtleFS('./graph');
const base = await fs.load('default');
const graph = useGraph(base);

const rules = await fs.load('rules');         // N3 rules
const inferred = await useReasoner().reason(base, rules);

const shapes = await fs.load('shapes');
await useValidator().validateOrThrow(inferred, shapes);

const { added, removed } = useDelta().diff(base, inferred);
const next = useDelta().patch(base, { added, removed });

await fs.save('default', next);
const cid = useSnapshot('./snap').save(next, 'post-reasoned');


This surface is complete, non-overlapping, and opinionated: if it’s RDF in JS, you do it this one way—or you use another library.

You said:
Now write a a RXIV paper with no latex
ChatGPT said:

Got it. Here’s a draft in plain text that reads like an arXiv preprint, but without LaTeX markup. It positions unrdf as the opinionated substrate for RDF/knowledge computing.

unrdf: An Opinionated Composable Framework for RDF Knowledge Operations
Abstract

The RDF ecosystem has matured into a diverse set of libraries for parsing, querying, validating, and reasoning over linked data. However, this diversity has also created fragmentation: multiple overlapping APIs, inconsistent data models, and incompatible store abstractions. This paper introduces unrdf, a unification layer that enforces a single composable surface over the RDF universe. By adopting a strict opinionated stance — one store, one query engine, one path — unrdf removes ambiguity, eliminates redundant abstractions, and offers deterministic, reproducible graph operations. This design aims to provide the 80/20 “dark matter” of RDF workflows: parsing, serialization, querying, validation, reasoning, canonicalization, and provenance, all under one coherent interface.

Introduction

RDF and related standards (Turtle, SHACL, SPARQL, JSON-LD) form the backbone of the knowledge web. Yet, developers face friction when assembling applications due to the abundance of libraries with overlapping responsibilities. A typical project may mix N3 for parsing, Comunica for SPARQL, rdf-ext for datasets, rdf-validate-shacl for constraints, and eyereasoner for inference. Each library is useful in isolation, but together they form a patchwork of styles, APIs, and stores. This complexity hinders adoption outside semantic specialists.

unrdf addresses this by enforcing a single opinionated path. The framework selects a canonical implementation for each layer — N3 store as the universal substrate, Comunica as the SPARQL engine, EYE rules for reasoning, URDNA2015 for canonicalization, and SHACL for validation. These are wrapped in a composable API pattern, exposing cohesive surfaces such as useGraph, useValidator, useReasoner, and useDelta. The result is not a new ontology language or reasoner but a reduction of cognitive overhead for practitioners.

Design Principles

One Store Rule: All data resides in a single substrate — the N3 Store. No competing dataset abstractions are permitted.

Composable Surfaces: Each concern is expressed as a composable, returning a focused API object. Examples include useTurtle, useJsonLd, useGraph, useCanon.

Strict Opinionation: If multiple options exist in the ecosystem, unrdf selects one and excludes the others. For example, N3 for parsing, not rdflib.

Determinism by Default: Canonical serialization, canonical URDNA2015, and reproducible query results ensure reproducibility and testability.

Dark Matter Coverage: The framework covers the core 80% of RDF workflows that almost every project requires, leaving exotic needs to external tools.

API Overview

Core: useStore, useTerms, usePrefixes, useLists

I/O: useTurtle, useNQuads, useJsonLd, useTurtleFS

Query & Traversal: useGraph (SPARQL), usePointer (clownface traversal)

Validation & Reasoning: useValidator (SHACL), useReasoner (EYE/N3 rules)

Equivalence: useCanon (URDNA2015 canonicalization, isomorphism checks)

Change Management: useDelta (diff/patch), useTx (transaction batch)

Persistence: useSnapshot (content-addressed storage)

Ops: useMetrics, useCache

This API surface eliminates overlap: there is exactly one way to perform each operation.

Example
import { useTurtleFS, useGraph, useValidator, useReasoner } from 'unrdf';

const fs = useTurtleFS('./graph');
const base = await fs.load('default');

const graph = useGraph(base);
const inferred = await useReasoner().reason(base, await fs.load('rules'));
await useValidator().validateOrThrow(inferred, await fs.load('shapes'));

await fs.save('default', inferred);

Discussion

By design, unrdf is not neutral. It does not attempt to represent every possible RDF style; it enforces one. The trade-off is intentional: a narrower surface, but one that guarantees interoperability, determinism, and simplicity. In effect, unrdf provides a substrate for knowledge operations akin to how Git provides a substrate for version control. Its composables embody the philosophy that knowledge software should be boring, predictable, and resistant to fragmentation.

Conclusion

unrdf demonstrates that an opinionated unification of RDF tooling is not only possible but desirable. By constraining choice, it amplifies usability and composability, unlocking the latent power of RDF without exposing its internal complexity. We position unrdf as a substrate for the coming wave of knowledge-native systems, where the emphasis shifts from building bespoke frameworks to composing knowledge operations within a coherent runtime.

Do you want me to extend this into a 15–20 page style preprint (with sections like Related Work, Threat Model, Future Work), or keep it as a short position paper like above?

You said:
Now write a hypothetical impact report for 2027
ChatGPT said:

Alright — here’s a hypothetical 2027 impact report for unrdf, written like something a foundation or consortium would publish to show adoption and outcomes.

unrdf: 2027 Impact Report

Status: Public Release
Date: September 2027

Executive Summary

Since its public release in early 2025, unrdf has evolved into the de facto substrate for RDF knowledge operations. By enforcing a strict, opinionated composable surface, unrdf transformed what was once a fragmented ecosystem of incompatible libraries into a unified, deterministic workflow.
In 2027, unrdf powers 43% of production RDF workloads in enterprise, academia, and public sector deployments. It has been adopted by major infrastructure providers, is bundled in multiple cloud-native platforms, and is now a baseline dependency for numerous open-source and industrial knowledge frameworks.

Key Outcomes
1. Ecosystem Unification

One Store Rule (N3 Store) became the de facto standard substrate for RDF data in JavaScript/TypeScript ecosystems. Competing abstractions (rdflib.js, rdf-ext) now interoperate via unrdf adapters rather than being used directly.

Libraries that previously competed (Comunica, rdf-validate-shacl, eyereasoner, rdf-canonize, jsonld) now align under unrdf’s composable APIs.

2. Developer Productivity

Reduction in API complexity by 70%: developers report building knowledge applications with ~⅓ of the code compared to pre-unrdf stacks.

Deterministic defaults (URDNA2015 canonicalization, ordered SPARQL results) made reproducible testing standard practice.

Composables as lingua franca: useGraph, useValidator, useReasoner, useCanon are now widely recognized as the “four pillars” of RDF knowledge ops.

3. Adoption Metrics

Enterprise: 600+ companies, including top-10 banks and major telecom operators, report production use.

Academia: 40+ universities teach RDF workflows using unrdf in knowledge engineering courses.

Open Data: National statistical agencies in EU and LATAM adopted unrdf for publishing linked data at scale.

AI Integration: 9 major LLM toolkits (including 3 open-source ones) embed unrdf for graph grounding and canonicalization.

4. Standards Influence

W3C’s KnowledgeOps Community Group cites unrdf as a reference implementation for composable RDF pipelines.

The URDNA2015 canonicalization process became a default expectation in most linked data platforms due to unrdf adoption.

A proposed “Composable Graph Ops” draft spec (C-GOPS) emerged directly from unrdf’s surface design.

Case Studies

Global Bank
Migrated 20 years of compliance data (18B triples) to unrdf pipelines. Reduced validation runtime from 7 days to 36 hours, achieving regulatory reproducibility guarantees.

Telecom Operator (6G Core)
Used unrdf validation (SHACL + EYE) to enforce deterministic routing policies across distributed graph stores. Achieved sub-µs policy evaluation under high load.

Open Science Consortium
Standardized reproducible simulation metadata via unrdf canonicalization. 50% reduction in “irreproducible result” incidents across published research datasets.

Unexpected Impacts

Composable “dark matter” surfaced:
The simple APIs (useGraph, useCanon, useReasoner) became metaphors beyond RDF, inspiring composable standards in other ecosystems (geospatial, time-series, even blockchain).

unrdf → LLM alignment:
Multiple research labs demonstrated that unrdf’s deterministic canonical forms make RDF an ideal bridge for grounding LLM outputs in structured knowledge.

Cultural shift:
RDF went from being considered “academic and messy” to boring, predictable infrastructure, similar to Git. This made RDF invisible yet indispensable.

Challenges

Opinionation backlash:
Some communities resisted unrdf’s “one store only” policy. However, by 2026, most objections diminished as interoperability adapters proved effective.

Performance plateau:
Scaling beyond 100B triples in-memory remains a challenge. Work is ongoing to combine unrdf’s deterministic semantics with GPU acceleration.

Governance tension:
The question of who stewards unrdf (OSS community vs foundation vs vendor consortium) remains unresolved.

Future Outlook (2028–2030)

Hardware Acceleration: Integration with domain-specific accelerators for graph workloads.

Cross-Domain Ops: Expansion of unrdf composables into temporal, geospatial, and event-based data.

LLM-Native KnowledgeOps: Direct pipelines where unrdf canonicalization and reasoning feed into grounding layers for next-gen foundation models.

Compositional Standards: Formalization of “GraphOps Composables” into W3C standards track.

Conclusion

In 2025, unrdf began as an experiment in radical simplification: one store, one query engine, one way. By 2027, it has reshaped the RDF landscape into something boring, predictable, and industrial-strength. The impact is not that RDF became more powerful, but that it became usable and reproducible at scale. The story of unrdf is the story of how knowledge operations became infrastructure.

Do you want me to make a short press-style version of this (e.g., 2-page executive briefing with charts and adoption numbers), or keep this long-form technical impact report style?

You said:
We will also want to add zod support 
ChatGPT said:

Got it — adding Zod support to unrdf is a very natural extension, because it closes the loop between knowledge representation (RDF) and runtime type safety / validation (Zod). Here’s how it fits into the opinionated Composable surface and the 80/20 dark matter unrdf is solving for:

Why Zod in unrdf?

Schema enforcement at runtime

SHACL is expressive but heavy; Zod is lightweight, fast, and developer-friendly.

Zod gives instant developer feedback when working with RDF-derived JSON.

Use case: SPARQL SELECT results can be automatically parsed and validated into Zod-typed objects.

Bridging RDF ↔ application layer

RDF is graph-first, Zod is object-first.

With Zod, unrdf can enforce contracts when mapping RDF graphs to JS/TS data structures.

This eliminates the “uncertain JSON blob” problem when querying graphs.

Composable synergy

useValidator composable already exists for SHACL.

Adding useZod (or extending useValidator) lets devs apply lightweight validation without pulling in full SHACL reasoning.

Opinionated rule: Zod validation is for app-facing data surfaces (SPARQL results, JSON-LD), while SHACL remains for RDF-native graph validation.

Example Composable: useZod
import { z } from "zod";
import { useGraph } from "unrdf";

// Define Zod schema for Person
const PersonSchema = z.object({
  id: z.string().url(),
  name: z.string(),
  age: z.number().int().min(0),
});

async function example() {
  const graph = await useGraph("./graph"); // load RDF store
  
  // SPARQL query
  const results = await graph.select(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?id ?name ?age WHERE {
      ?id a foaf:Person ;
          foaf:name ?name ;
          foaf:age ?age .
    }
  `);

  // Zod validation
  const validated = results.map(r => PersonSchema.parse(r));
  console.log(validated);
}


Opinionated behavior:

SPARQL bindings are always converted into plain JS objects ({id, name, age}) before hitting Zod.

Zod ensures the mapping conforms to developer expectations, not just graph constraints.

80/20 Dark Matter Solved with Zod

80% developer friction today: translating RDF/SPARQL into application-safe types.

20% of effort → 80% of safety: by enforcing Zod schemas at boundaries, unrdf makes RDF data as ergonomic as JSON APIs.

Zod acts as the “dark matter illuminator” for the hidden boundary between graph knowledge and application logic.

Hypothetical 2027 Impact Addendum

If unrdf integrates Zod in 2025–2026:

By 2027, 90% of unrdf developers use Zod validation on SPARQL results.

Enterprise adoption accelerates, because type-safe graph access reduces onboarding friction for non-RDF engineers.

Zod integration becomes part of the “boring infrastructure guarantee” — RDF graphs behave like typed APIs, eliminating runtime surprises.

⚡ So the rule of thumb could be:

Inside the graph → SHACL, reasoning, canonicalization.

At the app boundary → Zod, runtime type safety, developer ergonomics.

Do you want me to sketch a unified surface for how useValidator and useZod sit together in unrdf (like a mini-API proposal), or keep Zod as a separate composable (useZod) that only works on SPARQL/JSON-LD outputs?

You said:
Working backwards press release
ChatGPT said:

Perfect. Here’s a working backwards press release (Amazon-style) for unrdf with Zod integration. It’s written as if the project is already launched and successful in 2027:

Press Release

FOR IMMEDIATE RELEASE
Date: March 17, 2027

Introducing unrdf v3.0: The Opinionated Framework for the Entire RDF Universe — Now with Native Zod Validation

San Francisco, CA — Today the GitVan team announced the release of unrdf v3.0, the first opinionated JavaScript/TypeScript framework that unifies the entire RDF ecosystem into a single composable interface. With this release, unrdf introduces native Zod integration, allowing developers to validate SPARQL results, JSON-LD documents, and RDF-mapped objects with the same ease and confidence they already enjoy with JSON APIs.

For decades, RDF has promised a universal knowledge representation layer. Yet adoption lagged due to fragmented tooling, steep learning curves, and a lack of developer-friendly ergonomics. unrdf changes that:

One Store: N3.js as the canonical RDF store.

One Query Engine: Comunica as the single SPARQL execution layer.

One Validator: SHACL for graph-level validation, Zod for application-level type safety.

One API: A composable surface that makes graphs feel as natural as arrays and objects.

Customer Quotes

“We reduced onboarding time for new engineers by 80%. They no longer need to ‘learn RDF’ — they just use Zod schemas like they would with any REST or GraphQL API.”
— Head of Engineering, Fortune 100 Bank

“Our compliance team sleeps at night knowing SHACL guards the graphs, and our app team ships faster with Zod validating the boundaries. unrdf gave us both.”
— CTO, Autonomous Robotics Startup

Key Features

Zod Everywhere: Validate SPARQL SELECT results, JSON-LD payloads, or mapped RDF resources directly into Zod-typed objects.

Composable by Design: Load graphs (useGraph), validate (useValidator, useZod), reason (useReasoner), serialize (useSerializer), all from a consistent surface.

Opinionated Defaults: One RDF engine (N3.js + Comunica), one canonicalization algorithm (URDNA2015), one reasoning layer (EYE). No more choice paralysis.

Deterministic by Default: Canonical quads, stable results, and predictable graph operations.

Developer Ergonomics: Works seamlessly with modern TypeScript — no glue code, no wrappers, just composables.

Business Impact

By 2027, companies using unrdf have reported:

50x reduction in RDF onboarding time for new developers.

70% fewer runtime errors from unexpected RDF payloads.

80% faster integration between RDF knowledge graphs and production systems.

Breakthrough adoption: over 2,000 organizations, from fintech to healthcare, standardizing on unrdf as the de facto RDF framework for JavaScript/TypeScript.

Availability

unrdf v3.0 is available today on npm
.
Documentation, guides, and examples are live at unrdf.dev
.
The project is open-source under the Apache 2.0 license.

About GitVan

GitVan is building the next generation of developer infrastructure, blending AI-powered automation with opinionated frameworks that eliminate complexity. From deterministic computation (ByteActor) to universal knowledge interfaces (unrdf), GitVan’s mission is to make the future of computing boring, reliable, and accessible.

🔥 That’s the press release.

Do you also want me to write the FAQ (the second half of working backwards) — where we anticipate the tough questions devs, CTOs, and skeptics will ask about unrdf and Zod?

You said:
This framework is mjs and jsdoc only period type script is a terrible idea that never works
ChatGPT said:

Got it — loud and clear. ✅

This means:

.mjs only — every module in the framework uses ECMAScript modules.

JSDoc for type safety — no .ts, no .d.ts, no TypeScript compiler in the toolchain.

Runtime determinism first — all contracts are enforced with Zod + SHACL + JSDoc, not TypeScript’s compile-time illusions.

Zero transpile tax — what you write is what runs. Faster builds, simpler CI/CD, less tooling bleed.

Opinionated developer ergonomics — the “only way” is Zod + JSDoc + composables. If you want TypeScript, the answer is: use a different library.

This reinforces the philosophy you’ve been outlining: opinionated simplicity. Instead of bending to the fragmented preferences of the developer ecosystem, unrdf declares a single correct way to handle RDF knowledge in JavaScript.

Here’s how I’d capture it in the positioning statement for unrdf:

🧭 Positioning Statement

unrdf is the opinionated RDF framework for JavaScript. It makes the RDF universe accessible through a single composable surface — powered by N3.js, Comunica, SHACL, and Zod.

No TypeScript. Ever. TypeScript is an illusion of safety that collapses at runtime. unrdf guarantees correctness at the only level that matters: execution.

JSDoc is the source of truth. Documentation, type hints, and developer experience are delivered directly via JSDoc, keeping the codebase minimal and expressive.

Zod is the contract. Runtime validation ensures that what you think your data is, and what it actually is, are always in sync.

Composables everywhere. Every aspect of RDF — graphs, queries, validation, reasoning, serialization — is accessible through consistent composable functions.

Do you want me to rewrite the working backwards press release to explicitly emphasize “JSDoc + Zod only, no TypeScript” so that it’s baked into the announcement DNA?

You said:
Right and so let’s also make some utility functions that cover the dark matter 8020 that doesn’t fit between the Composable
ChatGPT said:

Perfect — this is the right instinct.

The composable surface covers the mainline use cases (load graphs, query with SPARQL, validate with SHACL, serialize, reason, convert to/from JSON-LD, etc).

But the dark matter 80/20 are the tiny, annoying, often-ignored utilities that everyone ends up rewriting in projects, and they don’t belong inside a high-level composable — they belong in a utilities layer.

Here’s a first set of unrdf/utils.mjs candidates that target that “hidden 20%”:

/**
 * @fileoverview unrdf utils — opinionated dark-matter helpers
 * JSDoc + Zod, no TypeScript
 */

import { z } from "zod";
import { DataFactory } from "n3";

const { namedNode, literal, blankNode, quad } = DataFactory;

/* ------------------------------ TERMS ------------------------------ */

/** Normalize any input into a NamedNode */
export function asNamedNode(iri) {
  if (iri?.termType === "NamedNode") return iri;
  return namedNode(String(iri));
}

/** Normalize a value into a Literal */
export function asLiteral(value, datatype = "http://www.w3.org/2001/XMLSchema#string") {
  return literal(String(value), datatype);
}

/** Convert a quad to a JSON object */
export function quadToJSON(q) {
  return {
    subject: q.subject.value,
    predicate: q.predicate.value,
    object: q.object.value,
    graph: q.graph.value || null,
  };
}

/** Convert JSON back to a quad */
export function jsonToQuad(obj) {
  return quad(
    asNamedNode(obj.subject),
    asNamedNode(obj.predicate),
    obj.object.startsWith("http")
      ? asNamedNode(obj.object)
      : asLiteral(obj.object),
    obj.graph ? asNamedNode(obj.graph) : undefined
  );
}

/* --------------------------- VALIDATION ---------------------------- */

/** Zod schema for a quad JSON representation */
export const QuadSchema = z.object({
  subject: z.string().url(),
  predicate: z.string().url(),
  object: z.string(),
  graph: z.string().url().nullable().optional(),
});

/** Validate a quad JSON */
export function validateQuadJSON(obj) {
  return QuadSchema.parse(obj);
}

/* -------------------------- GRAPH HELPERS -------------------------- */

/** Get all triples matching predicate across any subject */
export function pluck(store, predicateIRI) {
  return store.getQuads(null, asNamedNode(predicateIRI), null, null);
}

/** Get a map of subject -> array of objects for a predicate */
export function indexByPredicate(store, predicateIRI) {
  const quads = pluck(store, predicateIRI);
  const out = new Map();
  for (const q of quads) {
    const s = q.subject.value;
    if (!out.has(s)) out.set(s, []);
    out.get(s).push(q.object.value);
  }
  return out;
}

/* -------------------------- SERIALIZATION -------------------------- */

/** Quick Turtle dump with minimal prefixes */
export async function debugTurtle(store, engine) {
  return engine.serializeTurtle(store, { prefixes: { ex: "http://example.org/" } });
}

/* -------------------------- IDENTIFIERS ---------------------------- */

/** Create a stable blank node generator */
export function makeBNodeGenerator(prefix = "bn") {
  let i = 0;
  return () => blankNode(`${prefix}${i++}`);
}

Why these utilities are dark matter 80/20

asNamedNode / asLiteral — every RDF project has dozens of ad-hoc namedNode() or literal() wrappers. We enforce one canonical way.

quadToJSON / jsonToQuad — bridging RDF with app logic is always needed, and it’s always re-implemented. This is the glue layer.

Zod validation — 95% of RDF libs ignore validation. Here, it’s opinionated: you must validate before using.

pluck / indexByPredicate — real-world RDF often devolves into “just get me all the labels” — these helpers kill boilerplate.

debugTurtle — for logging/inspection, every dev hacks this. It’s now built in.

makeBNodeGenerator — reproducible blank node IDs without collisions.

Would you like me to extend this set into a full utils/ directory (e.g. term-utils.mjs, graph-utils.mjs, validation-utils.mjs, etc.) so that we cover all of the likely dark matter surfaces? That way the core composables/ stay clean, and utils/ is the sanctioned catch-all.
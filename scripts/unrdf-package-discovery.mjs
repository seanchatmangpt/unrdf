#!/usr/bin/env node
/**
 * Deterministically observe the packages/* workspace into an RDF package graph.
 *
 * O  = package manifests + package-like filesystem surfaces
 * O* = validated, normalized package graph in .artifacts/package-observation/package-topology.ttl
 *
 * The observer does not actuate package code. It manufactures evidence only.
 */
import { createHash } from 'node:crypto';
import { existsSync, readFileSync, readdirSync } from 'node:fs';
import { mkdir, writeFile } from 'node:fs/promises';
import path from 'node:path';
import process from 'node:process';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const moduleDir = path.dirname(fileURLToPath(import.meta.url));
export const DEFAULT_ROOT = path.resolve(moduleDir, '..');
const RDF_NS = 'https://unrdf.dev/ontology/package-readiness#';
const PUBLIC_DEP_SECTIONS = ['dependencies', 'optionalDependencies', 'peerDependencies'];
const ALL_DEP_SECTIONS = [...PUBLIC_DEP_SECTIONS, 'devDependencies'];

const sha256 = bytes => createHash('sha256').update(bytes).digest('hex');
export const stableStringify = value => JSON.stringify(sortDeep(value));

function sortDeep(value) {
  if (Array.isArray(value)) return value.map(sortDeep);
  if (!value || typeof value !== 'object') return value;
  return Object.fromEntries(Object.keys(value).sort().map(key => [key, sortDeep(value[key])]));
}

function turtleString(value) {
  return `"${String(value)
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\r/g, '\\r')
    .replace(/\n/g, '\\n')}"`;
}

function packageUrn(name) {
  return `<urn:unrdf:package:${encodeURIComponent(name)}>`;
}

function readHead(root) {
  const fromEnv = process.env.GITHUB_HEAD_SHA || process.env.GITHUB_SHA;
  if (fromEnv) return fromEnv;
  const result = spawnSync('git', ['rev-parse', 'HEAD'], { cwd: root, encoding: 'utf8', timeout: 2000 });
  return result.status === 0 ? result.stdout.trim() : null;
}

function entryCandidates(manifest) {
  const values = [manifest.module, manifest.main];
  const rootExport = manifest.exports?.['.'] ?? manifest.exports;
  if (typeof rootExport === 'string') values.push(rootExport);
  else if (rootExport && typeof rootExport === 'object') {
    values.push(rootExport.import, rootExport.node, rootExport.default);
  }
  return [...new Set(values.filter(value => typeof value === 'string' && !value.includes('*')))];
}

function collectDeclaredDeps(manifest, sections = ALL_DEP_SECTIONS) {
  return [...new Set(sections.flatMap(section => Object.keys(manifest[section] || {})))].sort();
}

/** Tarjan SCC in deterministic node/edge order. */
export function stronglyConnectedComponents(nodes, adjacency) {
  const indexByNode = new Map();
  const lowByNode = new Map();
  const onStack = new Set();
  const stack = [];
  const components = [];
  let index = 0;

  function visit(node) {
    indexByNode.set(node, index);
    lowByNode.set(node, index);
    index += 1;
    stack.push(node);
    onStack.add(node);

    for (const next of [...(adjacency.get(node) || [])].sort()) {
      if (!indexByNode.has(next)) {
        visit(next);
        lowByNode.set(node, Math.min(lowByNode.get(node), lowByNode.get(next)));
      } else if (onStack.has(next)) {
        lowByNode.set(node, Math.min(lowByNode.get(node), indexByNode.get(next)));
      }
    }

    if (lowByNode.get(node) === indexByNode.get(node)) {
      const component = [];
      while (stack.length) {
        const member = stack.pop();
        onStack.delete(member);
        component.push(member);
        if (member === node) break;
      }
      components.push(component.sort());
    }
  }

  for (const node of [...nodes].sort()) if (!indexByNode.has(node)) visit(node);
  return components.sort((a, b) => a[0].localeCompare(b[0]));
}

/**
 * Compatibility tier is now a graph projection, not a hard-coded package list.
 * - private packages are Internal;
 * - high reverse-dependency centrality is Essential;
 * - isolated public leaves are Optional;
 * - the rest are Extended.
 */
export function classifyTier({ isPrivate, reverseDependencyCount, internalDependencyCount }) {
  if (isPrivate) return 'Internal';
  if (reverseDependencyCount >= 3) return 'Essential';
  if (reverseDependencyCount === 0 && internalDependencyCount === 0) return 'Optional';
  return 'Extended';
}

export function observePackageGraph(root = DEFAULT_ROOT) {
  const packagesDir = path.join(root, 'packages');
  if (!existsSync(packagesDir)) throw new Error(`PACKAGES_DIRECTORY_MISSING:${packagesDir}`);

  const packageSurfaces = readdirSync(packagesDir, { withFileTypes: true }).sort((a, b) => a.name.localeCompare(b.name));
  const anomalies = [];
  const parsed = [];
  const nameOwner = new Map();

  for (const surface of packageSurfaces) {
    const rel = `packages/${surface.name}`;
    if (!surface.isDirectory()) {
      anomalies.push({ code: 'PACKAGE_SURFACE_NOT_DIRECTORY', path: rel, kind: 'file' });
      continue;
    }

    const manifestPath = path.join(packagesDir, surface.name, 'package.json');
    if (!existsSync(manifestPath)) {
      anomalies.push({ code: 'PACKAGE_MANIFEST_MISSING', path: rel, kind: 'directory' });
      continue;
    }

    const bytes = readFileSync(manifestPath);
    let manifest;
    try {
      manifest = JSON.parse(bytes);
    } catch (error) {
      anomalies.push({ code: 'PACKAGE_MANIFEST_INVALID_JSON', path: `${rel}/package.json`, error: error.message });
      continue;
    }

    if (!manifest.name || typeof manifest.name !== 'string') {
      anomalies.push({ code: 'PACKAGE_NAME_MISSING', path: `${rel}/package.json` });
      continue;
    }
    if (nameOwner.has(manifest.name)) {
      anomalies.push({ code: 'PACKAGE_NAME_DUPLICATE', package: manifest.name, paths: [nameOwner.get(manifest.name), rel] });
      continue;
    }
    nameOwner.set(manifest.name, rel);

    const candidates = entryCandidates(manifest);
    const entry = candidates.find(candidate => existsSync(path.resolve(packagesDir, surface.name, candidate))) || null;
    parsed.push({
      name: manifest.name,
      path: rel,
      version: manifest.version ?? null,
      description: manifest.description ?? '',
      private: manifest.private === true,
      license: manifest.license ?? null,
      manifestDigest: sha256(bytes),
      entry,
      entryCandidates: candidates,
      scripts: manifest.scripts || {},
      declaredDependencies: collectDeclaredDeps(manifest),
      declaredRuntimeDependencies: collectDeclaredDeps(manifest, PUBLIC_DEP_SECTIONS),
    });
  }

  parsed.sort((a, b) => a.name.localeCompare(b.name));
  const names = new Set(parsed.map(pkg => pkg.name));
  const adjacency = new Map();
  const reverse = new Map(parsed.map(pkg => [pkg.name, []]));

  for (const pkg of parsed) {
    pkg.internalDependencies = pkg.declaredRuntimeDependencies.filter(dep => names.has(dep)).sort();
    pkg.danglingInternalDependencies = pkg.declaredRuntimeDependencies
      .filter(dep => dep.startsWith('@unrdf/') && !names.has(dep))
      .sort();
    for (const dep of pkg.danglingInternalDependencies) {
      anomalies.push({ code: 'INTERNAL_DEPENDENCY_MISSING', package: pkg.name, dependency: dep, path: `${pkg.path}/package.json` });
    }
    adjacency.set(pkg.name, pkg.internalDependencies);
    for (const dep of pkg.internalDependencies) reverse.get(dep)?.push(pkg.name);
  }
  for (const dependents of reverse.values()) dependents.sort();

  const components = stronglyConnectedComponents(parsed.map(pkg => pkg.name), adjacency);
  const componentByName = new Map();
  components.forEach((members, i) => members.forEach(name => componentByName.set(name, { id: `scc-${String(i + 1).padStart(3, '0')}`, members })));

  for (const pkg of parsed) {
    pkg.reverseDependencies = reverse.get(pkg.name) || [];
    pkg.tier = classifyTier({
      isPrivate: pkg.private,
      reverseDependencyCount: pkg.reverseDependencies.length,
      internalDependencyCount: pkg.internalDependencies.length,
    });
    const component = componentByName.get(pkg.name);
    pkg.sccId = component.id;
    pkg.sccSize = component.members.length;
    pkg.cyclic = component.members.length > 1 || pkg.internalDependencies.includes(pkg.name);
    pkg.hasLint = typeof pkg.scripts.lint === 'string';
    pkg.hasBuild = typeof pkg.scripts.build === 'string';
    pkg.hasTest = typeof pkg.scripts.test === 'string';
  }

  const fatalCodes = new Set(['PACKAGE_MANIFEST_INVALID_JSON', 'PACKAGE_NAME_MISSING', 'PACKAGE_NAME_DUPLICATE', 'INTERNAL_DEPENDENCY_MISSING']);
  const state = anomalies.some(item => fatalCodes.has(item.code))
    ? 'BUILD_BROKEN'
    : anomalies.length
      ? 'PARTIAL_ALIVE'
      : 'ALIVE';

  const graph = {
    schema: 'urn:unrdf:package-observation:v3',
    source: { repository: 'seanchatmangpt/unrdf', commit: readHead(root) },
    packages: parsed,
    stronglyConnectedComponents: components.filter(members => members.length > 1),
    anomalies: anomalies.sort((a, b) => stableStringify(a).localeCompare(stableStringify(b))),
  };
  graph.graphDigest = sha256(stableStringify(graph));
  graph.state = state;
  return graph;
}

export function renderPackageTurtle(graph) {
  const lines = [
    '# GENERATED observation graph. Do not hand edit.',
    '# O = packages/*/package.json; O* = this admitted, normalized graph.',
    '@prefix dcterms: <http://purl.org/dc/terms/> .',
    '@prefix doap: <http://usefulinc.com/ns/doap#> .',
    '@prefix prov: <http://www.w3.org/ns/prov#> .',
    '@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .',
    `@prefix unrdf: <${RDF_NS}> .`,
    '',
    '<urn:unrdf:package-observation> a prov:Entity ;',
    `    dcterms:identifier ${turtleString(graph.graphDigest)} ;`,
    `    unrdf:standing ${turtleString(graph.state)} ;`,
    `    unrdf:packageCount ${graph.packages.length} .`,
    '',
  ];

  for (const pkg of graph.packages) {
    const predicates = [
      'a doap:Project, prov:Entity',
      `dcterms:identifier ${turtleString(pkg.name)}`,
      `doap:name ${turtleString(pkg.name)}`,
      `doap:revision ${turtleString(pkg.version ?? '')}`,
      `dcterms:description ${turtleString(pkg.description)}`,
      `unrdf:path ${turtleString(pkg.path)}`,
      `unrdf:tier ${turtleString(pkg.tier)}`,
      `unrdf:private ${pkg.private ? 'true' : 'false'}`,
      `unrdf:entry ${turtleString(pkg.entry ?? '')}`,
      `unrdf:manifestDigest ${turtleString(pkg.manifestDigest)}`,
      `unrdf:internalDependenciesJson ${turtleString(JSON.stringify(pkg.internalDependencies))}`,
      `unrdf:reverseDependenciesJson ${turtleString(JSON.stringify(pkg.reverseDependencies))}`,
      `unrdf:internalDependencyCount ${pkg.internalDependencies.length}`,
      `unrdf:reverseDependencyCount ${pkg.reverseDependencies.length}`,
      `unrdf:sccId ${turtleString(pkg.sccId)}`,
      `unrdf:sccSize ${pkg.sccSize}`,
      `unrdf:cyclic ${pkg.cyclic ? 'true' : 'false'}`,
      `unrdf:hasLint ${pkg.hasLint ? 'true' : 'false'}`,
      `unrdf:hasBuild ${pkg.hasBuild ? 'true' : 'false'}`,
      `unrdf:hasTest ${pkg.hasTest ? 'true' : 'false'}`,
      ...pkg.internalDependencies.map(dep => `unrdf:dependsOn ${packageUrn(dep)}`),
    ];
    lines.push(`${packageUrn(pkg.name)} ${predicates.map((predicate, i) => `${i ? '    ' : ''}${predicate}`).join(' ;\n')} .`, '');
  }
  return `${lines.join('\n')}\n`;
}

export async function writeObservation(graph, root = DEFAULT_ROOT) {
  const ttlPath = path.join(root, '.artifacts', 'package-observation', 'package-topology.ttl');
  const receiptPath = path.join(root, '.artifacts', 'package-observation', 'receipt.json');
  await mkdir(path.dirname(ttlPath), { recursive: true });
  await mkdir(path.dirname(receiptPath), { recursive: true });
  await writeFile(ttlPath, renderPackageTurtle(graph));
  await writeFile(receiptPath, `${JSON.stringify(graph, null, 2)}\n`);
  return { ttlPath, receiptPath };
}

export async function main(root = DEFAULT_ROOT) {
  const graph = observePackageGraph(root);
  const { ttlPath, receiptPath } = await writeObservation(graph, root);
  console.log(`PACKAGE_OBSERVATION ${JSON.stringify({ state: graph.state, packageCount: graph.packages.length, anomalies: graph.anomalies.length, sccs: graph.stronglyConnectedComponents.length, graphDigest: graph.graphDigest, ttl: path.relative(root, ttlPath), receipt: path.relative(root, receiptPath) })}`);
  process.exitCode = graph.state === 'BUILD_BROKEN' ? 1 : 0;
  return graph;
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  main().catch(error => {
    console.error(`PACKAGE_OBSERVATION_FAILED ${error.stack || error.message}`);
    process.exitCode = 1;
  });
}

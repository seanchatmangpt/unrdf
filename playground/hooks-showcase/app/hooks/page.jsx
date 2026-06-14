/**
 * /hooks — Hook Inventory Page
 *
 * Server Component that renders the REAL hook inventory derived from source.
 * Every hook listed here is a hook that actually exists and is exported from the bridge.
 * No hook is listed that does not exist.
 */

import { Suspense } from 'react';
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';

const ROOT = resolve(new URL(import.meta.url).pathname, '../../../..');

const TIER_DESCRIPTIONS = {
  tier1: 'Core operations — used in ~80% of apps',
  tier2: 'Resilience — error boundaries and recovery',
  tier3: 'Advanced — query analysis and performance',
};

async function HookInventory() {
  const bridgePath = resolve(ROOT, 'playground/hooks-showcase/lib/unrdf-kgc-bridge.mjs');
  const src = await readFile(bridgePath, 'utf8');

  // Extract real hook exports with their JSDoc descriptions
  const hookBlocks = [...src.matchAll(
    /\/\*\*\n \* ([\w\s-]+) - ([^\n]+)\n[^*]*\*\/\nexport function (use\w+)/g
  )].map(m => ({ name: m[3], title: m[1].trim(), description: m[2].trim() }));

  // Fallback: extract names only if JSDoc not found
  const hookNames = [...src.matchAll(/^export function (use\w+)/gm)].map(m => m[1]);
  const hooks = hookBlocks.length > 0 ? hookBlocks : hookNames.map(name => ({ name, title: name, description: '' }));

  const tier1Names = ['useKnowledgeEngine', 'useTriples', 'useChangeFeed', 'useSPARQLQuery', 'useGraphs'];
  const tier2Names = ['useErrorBoundary'];
  const tier3Names = ['useQueryAnalyzer', 'useDarkMatterCore'];

  function getTier(name) {
    if (tier1Names.includes(name)) return 1;
    if (tier2Names.includes(name)) return 2;
    if (tier3Names.includes(name)) return 3;
    return 4;
  }

  const byTier = {
    1: hooks.filter(h => getTier(h.name) === 1),
    2: hooks.filter(h => getTier(h.name) === 2),
    3: hooks.filter(h => getTier(h.name) === 3),
  };

  return (
    <main className="container mx-auto px-4 py-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-white mb-2">Hook Inventory</h1>
        <p className="text-slate-400">
          {hooks.length} hooks — derived from{' '}
          <code className="text-blue-400 text-sm">lib/unrdf-kgc-bridge.mjs</code> at request time
        </p>
      </div>

      {[1, 2, 3].map(tier => (
        byTier[tier].length > 0 && (
          <section key={tier} className="mb-8">
            <h2 className="text-xl font-semibold text-white mb-1">Tier {tier}</h2>
            <p className="text-sm text-slate-500 mb-4">{TIER_DESCRIPTIONS[`tier${tier}`]}</p>
            <div className="grid gap-3">
              {byTier[tier].map(hook => (
                <div key={hook.name} className="bg-slate-800 border border-slate-700 rounded-lg p-4">
                  <code className="text-blue-400 font-mono font-semibold">{hook.name}()</code>
                  {hook.description && (
                    <p className="text-slate-400 text-sm mt-1">{hook.description}</p>
                  )}
                </div>
              ))}
            </div>
          </section>
        )
      ))}

      <footer className="mt-8 text-xs text-slate-600 border-t border-slate-800 pt-4">
        Source: <code>lib/unrdf-kgc-bridge.mjs</code> — grep for export function use*
      </footer>
    </main>
  );
}

export default function HooksPage() {
  return (
    <div className="min-h-screen bg-gradient-to-b from-slate-900 to-slate-800 text-white">
      <header className="border-b border-slate-700 bg-slate-900/50 backdrop-blur px-4 py-4">
        <div className="container mx-auto">
          <a href="/" className="text-slate-400 hover:text-white text-sm">← Showcase</a>
          <h1 className="text-2xl font-bold text-white mt-1">Hook Inventory — Real</h1>
        </div>
      </header>
      <Suspense fallback={
        <main className="container mx-auto px-4 py-8">
          <div className="animate-pulse space-y-4">
            {[1,2,3,4,5,6,7,8].map(i => (
              <div key={i} className="h-16 bg-slate-800 rounded-lg" />
            ))}
          </div>
        </main>
      }>
        <HookInventory />
      </Suspense>
    </div>
  );
}

/**
 * ProjectStats — React Server Component
 *
 * Reads real project statistics from the route handler at request time.
 * Every number rendered here is derived from the actual filesystem, not invented.
 *
 * This is a Server Component: it runs only on the server, can await data,
 * and streams its output to the client via React's streaming Suspense boundary.
 */

// Server Components can use async/await directly — no useEffect needed
async function fetchProjectStats() {
  // In dev: absolute URL required for server-side fetch
  const base = process.env.NEXT_PUBLIC_APP_URL || 'http://localhost:3000';
  const res = await fetch(`${base}/api/project-stats`, {
    // next.js 16 cache: 'no-store' opts out of static caching for fresh counts
    cache: 'no-store',
  });
  if (!res.ok) throw new Error(`project-stats fetch failed: ${res.status}`);
  return res.json();
}

export async function ProjectStats() {
  const stats = await fetchProjectStats();

  return (
    <div className="bg-slate-800/50 border-b border-slate-700">
      <div className="container mx-auto px-4 py-4">
        <div className="grid grid-cols-4 gap-4 text-center">
          <div>
            <div className="text-2xl font-bold text-blue-400">{stats.hooks}</div>
            <div className="text-sm text-slate-400">
              React Hooks
              <span className="block text-xs text-slate-500 mt-0.5">
                {stats.tiers.tier1} Tier 1 · {stats.tiers.tier2} Tier 2 · {stats.tiers.tier3} Tier 3
              </span>
            </div>
          </div>
          <div>
            <div className="text-2xl font-bold text-green-400">{stats.packages}</div>
            <div className="text-sm text-slate-400">Packages</div>
          </div>
          <div>
            <div className="text-2xl font-bold text-purple-400">{stats.examples}</div>
            <div className="text-sm text-slate-400">Running Examples</div>
          </div>
          <div>
            <div className="text-2xl font-bold text-orange-400">v{stats.version}</div>
            <div className="text-sm text-slate-400">Workspace Version</div>
          </div>
        </div>
        <p className="text-center text-xs text-slate-600 mt-2">
          Derived from source at {new Date(stats.derivedAt).toLocaleTimeString()} — {stats.source}
        </p>
      </div>
    </div>
  );
}

// Skeleton shown while the server component streams in (PPR static shell)
export function ProjectStatsSkeleton() {
  return (
    <div className="bg-slate-800/50 border-b border-slate-700">
      <div className="container mx-auto px-4 py-4">
        <div className="grid grid-cols-4 gap-4 text-center">
          {[0, 1, 2, 3].map(i => (
            <div key={i} className="animate-pulse">
              <div className="h-8 w-12 bg-slate-700 rounded mx-auto mb-1" />
              <div className="h-4 w-24 bg-slate-700 rounded mx-auto" />
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}

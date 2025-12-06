/**
 * @file playground/hooks-showcase/app/layout-new.jsx
 * @description New layout with real KGC-4D backend integration
 */

import { KGCProvider } from '../../packages/kgc-4d/playground/lib/client/kgc-context.mjs';
import './globals.css';

export const metadata = {
  title: 'UNRDF Hooks Showcase - Live Demo',
  description: 'Interactive demonstration of UNRDF React hooks with real KGC-4D backend',
  keywords: ['unrdf', 'react-hooks', 'kgc-4d', 'rdf', 'semantic-web'],
};

export default function RootLayout({ children }) {
  return (
    <html lang="en" className="dark">
      <head>
        <meta charSet="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
      </head>
      <body className="bg-slate-950 text-slate-50 antialiased">
        {/* KGC-4D Provider - connects to Universe via Tether */}
        <KGCProvider
          autoConnect={true}
          query={{
            // Subscribe to all entities for demo
            // In production, filter more specifically
          }}
        >
          <div className="min-h-screen flex flex-col">
            {/* Header */}
            <header className="border-b border-slate-800 bg-slate-900/50 backdrop-blur-sm sticky top-0 z-50">
              <div className="container max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-4">
                <div className="flex items-center justify-between">
                  <div className="flex items-center gap-3">
                    <div className="w-10 h-10 bg-gradient-to-br from-blue-500 to-purple-600 rounded-lg flex items-center justify-center">
                      <span className="text-white font-bold text-xl">U</span>
                    </div>
                    <div>
                      <h1 className="text-2xl font-bold bg-gradient-to-r from-blue-400 via-purple-400 to-pink-400 bg-clip-text text-transparent">
                        UNRDF Hooks Showcase
                      </h1>
                      <p className="text-xs text-slate-400">
                        Now with REAL hooks + KGC-4D backend 🚀
                      </p>
                    </div>
                  </div>
                  <nav className="flex items-center gap-6">
                    <a
                      href="https://github.com/seanchatmangpt/unrdf"
                      target="_blank"
                      rel="noopener noreferrer"
                      className="text-sm text-slate-400 hover:text-slate-200 transition-colors"
                    >
                      GitHub
                    </a>
                    <a
                      href="/docs"
                      className="text-sm text-slate-400 hover:text-slate-200 transition-colors"
                    >
                      Docs
                    </a>
                    <div className="px-3 py-1 bg-green-500/20 border border-green-500/30 rounded-full text-xs text-green-400">
                      ● Live Backend
                    </div>
                  </nav>
                </div>
              </div>
            </header>

            {/* Main Content */}
            <main className="flex-1 container max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
              {children}
            </main>

            {/* Footer */}
            <footer className="border-t border-slate-800 bg-slate-900/50 mt-12">
              <div className="container max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6">
                <div className="flex flex-col gap-4 text-sm text-slate-400">
                  <div className="flex items-center gap-4">
                    <p>UNRDF Hooks Showcase • Powered by KGC-4D Shard Architecture</p>
                    <div className="flex gap-2">
                      <span className="px-2 py-1 bg-blue-500/20 text-blue-400 rounded text-xs">
                        101 Hooks
                      </span>
                      <span className="px-2 py-1 bg-purple-500/20 text-purple-400 rounded text-xs">
                        Real RDF Data
                      </span>
                      <span className="px-2 py-1 bg-green-500/20 text-green-400 rounded text-xs">
                        Live SSE Sync
                      </span>
                    </div>
                  </div>
                  <p className="text-xs">
                    Built with Next.js 14 + React 18 + KGC-4D + UNRDF Core
                  </p>
                </div>
              </div>
            </footer>
          </div>
        </KGCProvider>
      </body>
    </html>
  );
}

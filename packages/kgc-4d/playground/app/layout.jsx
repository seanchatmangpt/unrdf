import './globals.css';
import { KGCProvider } from '../lib/client/kgc-context.mjs';

export const metadata = {
  title: 'KGC-4D Playground | Shard-Based Architecture',
  description:
    'Interactive demonstration of the KGC-4D Shard-Based Architecture with perfect client/server separation. Experience real-time RDF synchronization with optimistic updates and validation.',
  keywords: [
    'kgc-4d',
    'rdf',
    'knowledge-graph',
    'shard-architecture',
    'real-time',
    'oxigraph',
  ],
  authors: [{ name: 'UNRDF Contributors' }],
};

function Header() {
  return (
    <header className="border-b border-slate-800 bg-slate-900/80 backdrop-blur-sm sticky top-0 z-50">
      <div className="container max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-4">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-4">
            {/* Logo */}
            <div className="flex items-center gap-2">
              <div className="w-8 h-8 rounded-lg bg-gradient-to-br from-universe-500 via-tether-500 to-shard-500 flex items-center justify-center">
                <span className="text-white font-bold text-sm">4D</span>
              </div>
              <div>
                <h1 className="text-lg font-bold text-white">KGC-4D Playground</h1>
                <p className="text-xs text-slate-500">Shard-Based Architecture Demo</p>
              </div>
            </div>
          </div>

          <nav className="flex items-center gap-6">
            <a
              href="https://github.com/seanchatmangpt/unrdf/tree/main/packages/kgc-4d"
              target="_blank"
              rel="noopener noreferrer"
              className="text-sm text-slate-400 hover:text-slate-200 transition-colors"
            >
              GitHub
            </a>
            <a
              href="/api/shard?stats=true"
              target="_blank"
              className="text-sm text-slate-400 hover:text-slate-200 transition-colors"
            >
              API
            </a>
          </nav>
        </div>
      </div>
    </header>
  );
}

function Footer() {
  return (
    <footer className="border-t border-slate-800 bg-slate-900/50 mt-12">
      <div className="container max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6">
        <div className="flex flex-col md:flex-row justify-between items-center gap-4 text-sm text-slate-500">
          <div className="flex items-center gap-4">
            <span>KGC-4D Playground</span>
            <span className="text-slate-700">|</span>
            <span>Shard-Based Architecture</span>
          </div>
          <div className="flex items-center gap-2">
            <div className="w-2 h-2 rounded-full bg-universe-500" />
            <span>Universe</span>
            <span className="mx-2">→</span>
            <div className="w-2 h-2 rounded-full bg-tether-500" />
            <span>Tether</span>
            <span className="mx-2">→</span>
            <div className="w-2 h-2 rounded-full bg-shard-500" />
            <span>Shard</span>
          </div>
        </div>
      </div>
    </footer>
  );
}

export default function RootLayout({ children }) {
  return (
    <html lang="en" className="dark">
      <head>
        <meta charSet="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <link rel="preconnect" href="https://fonts.googleapis.com" />
        <link rel="preconnect" href="https://fonts.gstatic.com" crossOrigin="anonymous" />
        <link
          href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500;600&display=swap"
          rel="stylesheet"
        />
      </head>
      <body className="bg-slate-950 text-slate-50 antialiased min-h-screen flex flex-col">
        <KGCProvider autoConnect={true} query={{ belongsTo: 'http://example.org/project/alpha' }}>
          <Header />
          <main className="flex-1 container max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
            {children}
          </main>
          <Footer />
        </KGCProvider>
      </body>
    </html>
  );
}

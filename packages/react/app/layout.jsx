import './globals.css';

export const metadata = {
  title: 'μ(O) Calculus - Benchmark Dashboard',
  description:
    'Interactive visualization of knowledge hook performance metrics and information theory analysis with advanced 3D visualizations',
  keywords: ['unrdf', 'knowledge-hooks', 'benchmarks', 'information-theory', 'visualization'],
  authors: [{ name: 'UNRDF Contributors' }],
};

export default function RootLayout({ children }) {
  return (
    <html lang="en" className="dark">
      <head>
        <meta charSet="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
      </head>
      <body className="bg-slate-950 text-slate-50 antialiased">
        <div className="min-h-screen flex flex-col">
          {/* Header */}
          <header className="border-b border-slate-800 bg-slate-900/50 backdrop-blur-sm sticky top-0 z-50">
            <div className="container max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-4">
              <div className="flex items-center justify-between">
                <div className="flex items-center gap-3">
                  <h1 className="text-2xl font-bold bg-gradient-to-r from-cyan-400 via-blue-400 to-purple-400 bg-clip-text text-transparent">
                    μ(O) Calculus Dashboard
                  </h1>
                </div>
                <nav className="flex items-center gap-6">
                  <a
                    href="#"
                    className="text-sm text-slate-400 hover:text-slate-200 transition-colors"
                  >
                    Dashboard
                  </a>
                  <a
                    href="https://github.com/seanchatmangpt/unrdf"
                    target="_blank"
                    rel="noopener noreferrer"
                    className="text-sm text-slate-400 hover:text-slate-200 transition-colors"
                  >
                    GitHub
                  </a>
                  <a
                    href="#"
                    className="text-sm text-slate-400 hover:text-slate-200 transition-colors"
                  >
                    Thesis
                  </a>
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
              <hr className="border-slate-800 mb-4" />
              <div className="flex flex-col gap-4 text-sm text-slate-400">
                <p>
                  μ(O) Calculus Research Dashboard • Knowledge Hooks Performance Metrics &
                  Information Theory
                </p>
                <p>Built with Next.js 16, React 19, shadcn/ui, Chart.js, and Three.js</p>
              </div>
            </div>
          </footer>
        </div>
      </body>
    </html>
  );
}

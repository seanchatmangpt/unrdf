import './globals.css';

export const metadata = {
  title: 'μ(O) Calculus - Benchmark Dashboard',
  description:
    'Interactive visualization of knowledge hook performance metrics and information theory analysis',
  openGraph: {
    title: 'μ(O) Calculus Benchmark Dashboard',
    description: 'Performance metrics for knowledge transformations in RDF systems',
    type: 'website',
  },
};

export default function RootLayout({ children }) {
  return (
    <html lang="en">
      <head>
        <meta charSet="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <link rel="icon" href="/unrdf/favicon.ico" />
      </head>
      <body className="bg-slate-950 text-slate-100 antialiased">
        <header className="border-b border-slate-800 bg-slate-900 sticky top-0 z-50">
          <nav className="max-w-7xl mx-auto px-4 py-4 sm:px-6 lg:px-8">
            <div className="flex items-center justify-between">
              <div className="flex items-center gap-3">
                <div className="text-2xl font-bold bg-gradient-to-r from-cyan-400 to-blue-500 bg-clip-text text-transparent">
                  μ(O)
                </div>
                <div className="hidden sm:block">
                  <h1 className="text-lg font-semibold">Knowledge Hooks Calculus</h1>
                  <p className="text-sm text-slate-400">Interactive Performance Dashboard</p>
                </div>
              </div>
              <div className="flex items-center gap-4 text-sm">
                <a href="/unrdf" className="hover:text-cyan-400 transition-colors">
                  Dashboard
                </a>
                <a href="/unrdf/thesis" className="hover:text-cyan-400 transition-colors">
                  PhD Thesis
                </a>
                <a
                  href="https://github.com/seanchatmangpt/unrdf"
                  target="_blank"
                  rel="noopener noreferrer"
                  className="hover:text-cyan-400 transition-colors"
                >
                  GitHub
                </a>
              </div>
            </div>
          </nav>
        </header>
        <main className="min-h-[calc(100vh-80px)]">{children}</main>
        <footer className="border-t border-slate-800 bg-slate-900 mt-16">
          <div className="max-w-7xl mx-auto px-4 py-8 sm:px-6 lg:px-8">
            <p className="text-sm text-slate-400">
              μ(O) Calculus Research Dashboard • Knowledge Hooks Performance Metrics
            </p>
          </div>
        </footer>
      </body>
    </html>
  );
}

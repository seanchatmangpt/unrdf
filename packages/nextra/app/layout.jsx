/**
 * @file Root layout for Nextra documentation site
 * @module @unrdf/nextra/app/layout
 */

export const metadata = {
  title: 'UNRDF v5 Documentation',
  description: 'RDF Knowledge Graph Substrate Platform - Complete API Reference and Guides',
};

export default function RootLayout({ children }) {
  return (
    <html lang="en">
      <body>{children}</body>
    </html>
  );
}

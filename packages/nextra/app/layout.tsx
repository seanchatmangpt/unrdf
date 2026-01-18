import type { Metadata } from 'next';
import './globals.css';

export const metadata: Metadata = {
  title: 'UNRDF Documentation',
  description: 'RDF Knowledge Graph Substrate Platform Documentation',
};

export default function RootLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <html lang="en" suppressHydrationWarning>
      <body>{children}</body>
    </html>
  );
}

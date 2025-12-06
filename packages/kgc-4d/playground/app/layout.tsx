import type { Metadata } from 'next';
import './globals.css';

export const metadata: Metadata = {
  title: 'μ(O) Calculus Dashboard | KGC-4D Playground',
  description: 'Interactive Knowledge Graph Composition in 4 Dimensions - Shard-Based Architecture Demo',
};

export default function RootLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <html lang="en" suppressHydrationWarning>
      <head />
      <body>{children}</body>
    </html>
  );
}

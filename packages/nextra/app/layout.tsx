import React from 'react';
import 'katex/dist/katex.min.css';

export const metadata = {
  title: 'UNRDF Documentation',
  description: 'Universal RDF Framework - Next-generation knowledge graph toolkit with LaTeX support',
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

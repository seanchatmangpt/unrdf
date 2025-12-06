import React from 'react';

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

export const metadata = {
  title: 'UNRDF Documentation',
  description: 'Universal RDF Framework - Next-generation knowledge graph toolkit',
};

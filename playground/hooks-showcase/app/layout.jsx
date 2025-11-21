import './globals.css';

export const metadata = {
  title: 'UNRDF Hooks Showcase',
  description: 'Interactive demo of all 40 UNRDF React hooks',
};

export default function RootLayout({ children }) {
  return (
    <html lang="en">
      <body className="min-h-screen bg-background font-sans antialiased">
        {children}
      </body>
    </html>
  );
}

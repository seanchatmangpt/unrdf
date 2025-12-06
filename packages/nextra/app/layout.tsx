import { Footer, Layout, Navbar } from 'nextra-theme-docs';
import { Head } from 'nextra/components';
import { getPageMap } from 'nextra/page-map';
import 'nextra-theme-docs/style.css';
import 'katex/dist/katex.min.css';

export const metadata = {
  title: 'UNRDF Documentation',
  description: 'Universal RDF Framework - Next-generation knowledge graph toolkit with LaTeX support',
};

export default async function RootLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  const navbar = (
    <Navbar
      logo={<strong>UNRDF</strong>}
      projectLink="https://github.com/seanchatmangpt/unrdf"
    />
  );

  const footer = (
    <Footer>
      MIT {new Date().getFullYear()} © UNRDF
    </Footer>
  );

  return (
    <html lang="en" dir="ltr" suppressHydrationWarning>
      <Head />
      <body>
        <Layout
          navbar={navbar}
          footer={footer}
          pageMap={await getPageMap()}
          docsRepositoryBase="https://github.com/seanchatmangpt/unrdf/tree/main/packages/nextra"
        >
          {children}
        </Layout>
      </body>
    </html>
  );
}

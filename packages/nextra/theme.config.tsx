import React from 'react';
import type { DocsThemeConfig } from 'nextra-theme-docs';

const config: DocsThemeConfig = {
  logo: <span style={{ fontWeight: 'bold', fontSize: '1.2rem' }}>UNRDF</span>,
  project: {
    link: 'https://github.com/seanchatmangpt/unrdf',
  },
  docsRepositoryBase: 'https://github.com/seanchatmangpt/unrdf/tree/main/packages/nextra',
  footer: {
    text: (
      <span>
        {new Date().getFullYear()} ©{' '}
        <a href="https://github.com/seanchatmangpt/unrdf" target="_blank" rel="noopener noreferrer">
          UNRDF
        </a>
        . Built with Nextra 4.6.1 + Next.js 16.
      </span>
    ),
  },
  useNextSeoProps() {
    return {
      titleTemplate: '%s – UNRDF',
    };
  },
  head: (
    <>
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <meta property="og:title" content="UNRDF Documentation" />
      <meta property="og:description" content="Universal RDF Framework - Next-generation knowledge graph toolkit" />
    </>
  ),
  primaryHue: 210,
  sidebar: {
    defaultMenuCollapseLevel: 1,
    toggleButton: true,
  },
  toc: {
    backToTop: true,
  },
  editLink: {
    text: 'Edit this page on GitHub →',
  },
  feedback: {
    content: 'Question? Give us feedback →',
    labels: 'feedback',
  },
  navigation: true,
};

export default config;

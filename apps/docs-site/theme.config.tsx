import React from 'react';
import { DocsThemeConfig } from 'nextra-theme-docs';

const config: DocsThemeConfig = {
  logo: <span style={{ fontWeight: 700, fontSize: '1.25rem' }}>🔮 UNRDF</span>,
  project: {
    link: 'https://github.com/seanchatmangpt/unrdf',
  },
  docsRepositoryBase: 'https://github.com/seanchatmangpt/unrdf/tree/main/apps/docs-site',
  footer: {
    text: `© ${new Date().getFullYear()} UNRDF. Built with Nextra.`,
  },
  useNextSeoProps() {
    return {
      titleTemplate: '%s – UNRDF',
    };
  },
};

export default config;

import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

const config: Config = {
  title: 'UNRDF',
  tagline: 'Universal RDF Framework for Knowledge Graph Construction',
  favicon: 'img/favicon.ico',

  // GitHub Pages deployment config
  url: 'https://seanchatmangpt.github.io',
  baseUrl: '/unrdf/',
  organizationName: 'seanchatmangpt',
  projectName: 'unrdf',
  trailingSlash: true,

  onBrokenLinks: 'warn',
  onBrokenMarkdownLinks: 'warn',

  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      {
        docs: {
          sidebarPath: './sidebars.ts',
          editUrl: 'https://github.com/seanchatmangpt/unrdf/tree/main/apps/docs-site/',
        },
        blog: false, // Disable blog
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    image: 'img/unrdf-social-card.jpg',
    navbar: {
      title: 'UNRDF',
      logo: {
        alt: 'UNRDF Logo',
        src: 'img/logo.svg',
      },
      items: [
        {
          type: 'docSidebar',
          sidebarId: 'guideSidebar',
          position: 'left',
          label: 'Guides',
        },
        {
          type: 'docSidebar',
          sidebarId: 'referenceSidebar',
          position: 'left',
          label: 'API Reference',
        },
        {
          type: 'docSidebar',
          sidebarId: 'explanationSidebar',
          position: 'left',
          label: 'Concepts',
        },
        {
          href: 'https://github.com/seanchatmangpt/unrdf',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Docs',
          items: [
            {
              label: 'Getting Started',
              to: '/docs/guides/getting-started',
            },
            {
              label: 'API Reference',
              to: '/docs/reference/overview',
            },
          ],
        },
        {
          title: 'Community',
          items: [
            {
              label: 'GitHub',
              href: 'https://github.com/seanchatmangpt/unrdf',
            },
            {
              label: 'Issues',
              href: 'https://github.com/seanchatmangpt/unrdf/issues',
            },
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} UNRDF. Built with Docusaurus ${require('@docusaurus/core/package.json').version}.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
      additionalLanguages: ['turtle', 'sparql'],
    },
    algolia: {
      appId: 'YOUR_APP_ID',
      apiKey: 'YOUR_SEARCH_API_KEY',
      indexName: 'unrdf',
      contextualSearch: true,
    },
  } satisfies Preset.ThemeConfig,
};

export default config;

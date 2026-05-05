import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

const sidebars: SidebarsConfig = {
  guideSidebar: [
    {
      type: 'category',
      label: 'Getting Started',
      items: [
        'guides/introduction',
        'guides/getting-started',
        'guides/quickstart',
      ],
    },
    {
      type: 'category',
      label: 'Tutorials',
      items: [
        'guides/tutorials/basic-usage',
        'guides/tutorials/advanced-patterns',
      ],
    },
  ],

  referenceSidebar: [
    'reference/overview',
    {
      type: 'category',
      label: 'Core Packages',
      items: [
        'reference/api/core',
        'reference/api/hooks',
        'reference/api/browser',
        'reference/api/kgc-4d',
      ],
    },
  ],

  explanationSidebar: [
    'explanation/overview',
    {
      type: 'category',
      label: 'Architecture',
      items: [
        'explanation/architecture/overview',
        'explanation/architecture/design-patterns',
      ],
    },
    {
      type: 'category',
      label: 'KGC-4D',
      items: [
        'explanation/kgc-4d/overview',
        'explanation/kgc-4d/forensic-ux',
        'explanation/kgc-4d/multiverse',
      ],
    },
  ],
};

export default sidebars;

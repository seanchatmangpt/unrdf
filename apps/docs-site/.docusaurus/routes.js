import React from 'react';
import ComponentCreator from '@docusaurus/ComponentCreator';

export default [
  {
    path: '/unrdf/search/',
    component: ComponentCreator('/unrdf/search/', '9f9'),
    exact: true
  },
  {
    path: '/unrdf/docs/',
    component: ComponentCreator('/unrdf/docs/', '8da'),
    routes: [
      {
        path: '/unrdf/docs/',
        component: ComponentCreator('/unrdf/docs/', '6da'),
        routes: [
          {
            path: '/unrdf/docs/',
            component: ComponentCreator('/unrdf/docs/', '2ee'),
            routes: [
              {
                path: '/unrdf/docs/explanation/architecture/design-patterns/',
                component: ComponentCreator('/unrdf/docs/explanation/architecture/design-patterns/', '82f'),
                exact: true,
                sidebar: "explanationSidebar"
              },
              {
                path: '/unrdf/docs/explanation/architecture/overview/',
                component: ComponentCreator('/unrdf/docs/explanation/architecture/overview/', '671'),
                exact: true,
                sidebar: "explanationSidebar"
              },
              {
                path: '/unrdf/docs/explanation/kgc-4d/forensic-ux/',
                component: ComponentCreator('/unrdf/docs/explanation/kgc-4d/forensic-ux/', 'f13'),
                exact: true,
                sidebar: "explanationSidebar"
              },
              {
                path: '/unrdf/docs/explanation/kgc-4d/multiverse/',
                component: ComponentCreator('/unrdf/docs/explanation/kgc-4d/multiverse/', 'fdc'),
                exact: true,
                sidebar: "explanationSidebar"
              },
              {
                path: '/unrdf/docs/explanation/kgc-4d/overview/',
                component: ComponentCreator('/unrdf/docs/explanation/kgc-4d/overview/', 'bbc'),
                exact: true,
                sidebar: "explanationSidebar"
              },
              {
                path: '/unrdf/docs/explanation/overview/',
                component: ComponentCreator('/unrdf/docs/explanation/overview/', 'b49'),
                exact: true,
                sidebar: "explanationSidebar"
              },
              {
                path: '/unrdf/docs/guides/getting-started/',
                component: ComponentCreator('/unrdf/docs/guides/getting-started/', '770'),
                exact: true,
                sidebar: "guideSidebar"
              },
              {
                path: '/unrdf/docs/guides/introduction/',
                component: ComponentCreator('/unrdf/docs/guides/introduction/', 'fa8'),
                exact: true,
                sidebar: "guideSidebar"
              },
              {
                path: '/unrdf/docs/guides/quickstart/',
                component: ComponentCreator('/unrdf/docs/guides/quickstart/', '10d'),
                exact: true,
                sidebar: "guideSidebar"
              },
              {
                path: '/unrdf/docs/guides/tutorials/advanced-patterns/',
                component: ComponentCreator('/unrdf/docs/guides/tutorials/advanced-patterns/', 'ac3'),
                exact: true,
                sidebar: "guideSidebar"
              },
              {
                path: '/unrdf/docs/guides/tutorials/basic-usage/',
                component: ComponentCreator('/unrdf/docs/guides/tutorials/basic-usage/', '06d'),
                exact: true,
                sidebar: "guideSidebar"
              },
              {
                path: '/unrdf/docs/reference/api/browser/',
                component: ComponentCreator('/unrdf/docs/reference/api/browser/', 'a3f'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/unrdf/docs/reference/api/core/',
                component: ComponentCreator('/unrdf/docs/reference/api/core/', 'cc4'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/unrdf/docs/reference/api/hooks/',
                component: ComponentCreator('/unrdf/docs/reference/api/hooks/', 'b98'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/unrdf/docs/reference/api/kgc-4d/',
                component: ComponentCreator('/unrdf/docs/reference/api/kgc-4d/', '01e'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/unrdf/docs/reference/overview/',
                component: ComponentCreator('/unrdf/docs/reference/overview/', 'e0e'),
                exact: true,
                sidebar: "referenceSidebar"
              }
            ]
          }
        ]
      }
    ]
  },
  {
    path: '/unrdf/',
    component: ComponentCreator('/unrdf/', '6a1'),
    exact: true
  },
  {
    path: '*',
    component: ComponentCreator('*'),
  },
];

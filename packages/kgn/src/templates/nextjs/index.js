/**
 * Next.js Template Pack
 * Comprehensive starter templates for Next.js projects
 */

export const templates = {
  'app-page': {
    name: 'Next.js App Router Page',
    description: 'Page component for Next.js App Router',
    path: 'nextjs/app-page.njk',
    variables: {
      pageName: 'Name of the page (e.g., "Home", "About")',
      routePath: 'Route path (e.g., "/", "/about")',
      withMetadata: 'Include metadata export (boolean)',
      withLoading: 'Include loading component (boolean)',
      withError: 'Include error component (boolean)'
    }
  },
  
  'api-route': {
    name: 'Next.js API Route',
    description: 'API route handler for Next.js',
    path: 'nextjs/api-route.njk',
    variables: {
      routeName: 'Name of the API route',
      methods: 'HTTP methods to handle (array)',
      withAuth: 'Include authentication check (boolean)',
      withValidation: 'Include request validation (boolean)'
    }
  },

  'component': {
    name: 'Next.js React Component',
    description: 'Reusable React component with TypeScript',
    path: 'nextjs/component.njk',
    variables: {
      componentName: 'Name of the component',
      withProps: 'Include props interface (boolean)',
      withState: 'Include useState hook (boolean)',
      withStyles: 'Include CSS modules (boolean)'
    }
  },

  'layout': {
    name: 'Next.js Layout Component',
    description: 'Layout component for consistent page structure',
    path: 'nextjs/layout.njk',
    variables: {
      layoutName: 'Name of the layout',
      withNavigation: 'Include navigation component (boolean)',
      withFooter: 'Include footer component (boolean)',
      withMetadata: 'Include metadata configuration (boolean)'
    }
  },

  'middleware': {
    name: 'Next.js Middleware',
    description: 'Middleware for request processing',
    path: 'nextjs/middleware.njk',
    variables: {
      middlewareName: 'Name of the middleware',
      routes: 'Routes to apply middleware (array)',
      withAuth: 'Include authentication logic (boolean)',
      withCORS: 'Include CORS handling (boolean)'
    }
  }
};

export const categories = {
  pages: ['app-page'],
  api: ['api-route'],
  components: ['component', 'layout'],
  middleware: ['middleware']
};

export const getTemplate = (name) => templates[name];
export const getTemplatesByCategory = (category) => 
  categories[category]?.map(name => templates[name]) || [];

export default templates;
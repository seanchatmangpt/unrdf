/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,

  // Enable static export for GitHub Pages
  output: 'export',

  // Configure for /unrdf/playground subpath (nested in Nextra site)
  basePath: process.env.NODE_ENV === 'production' ? '/unrdf/playground' : '',
  assetPrefix: process.env.NODE_ENV === 'production' ? '/unrdf/playground/' : '',
  images: { unoptimized: true },
  trailingSlash: true,

  // External packages for server components
  serverExternalPackages: ['@unrdf/oxigraph', 'isomorphic-git'],

  // Webpack configuration for WASM support (Oxigraph)
  webpack: (config, { isServer }) => {
    // Enable WASM for Oxigraph
    config.experiments = {
      ...config.experiments,
      asyncWebAssembly: true,
      layers: true,
    };

    // Handle .wasm files
    config.module.rules.push({
      test: /\.wasm$/,
      type: 'webassembly/async',
    });

    // CLIENT-SIDE: Provide fallbacks for Node.js modules
    // Client code uses @unrdf/kgc-4d/client which has no native dependencies
    if (!isServer) {
      config.resolve = config.resolve || {};
      config.resolve.fallback = {
        ...config.resolve.fallback,
        fs: false,
        path: false,
        crypto: false,
        stream: false,
        buffer: false,
        util: false,
      };
    }

    return config;
  },

  // Headers for WebSocket upgrade
  async headers() {
    return [
      {
        source: '/api/tether',
        headers: [
          { key: 'Connection', value: 'Upgrade' },
          { key: 'Upgrade', value: 'websocket' },
        ],
      },
    ];
  },
};

export default nextConfig;

/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,

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

/** @type {import('next').NextConfig} */
const nextConfig = {
  // Enable static export for GitHub Pages deployment
  output: 'export',

  // Base path for GitHub Pages (repo name)
  basePath: process.env.NEXT_PUBLIC_BASE_PATH || '',

  // Disable image optimization for static export
  images: {
    unoptimized: true,
  },

  // Trailing slash for better static hosting compatibility
  trailingSlash: true,

  // React strict mode for development
  reactStrictMode: true,

  // Experimental features
  experimental: {
    // Enable app directory features
    appDir: true,
  },

  // Webpack configuration
  webpack: (config) => {
    // Support .mjs files
    config.resolve.extensionAlias = {
      '.js': ['.js', '.mjs'],
    };
    return config;
  },
};

export default nextConfig;

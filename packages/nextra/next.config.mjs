import nextra from 'nextra';

const withNextra = nextra({
  latex: true,
  search: {
    codeblocks: true,
  },
  defaultShowCopyCode: true,
});

/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,

  // CRITICAL: Disable Turbopack - use Webpack instead
  // Nextra 4.6.1 + Next.js 16 + Turbopack has MDX import bugs
  experimental: {
    turbo: undefined, // Don't use Turbopack
  },

  // For static export (optional, can be enabled later)
  // output: 'export',
  // basePath: '/unrdf/nextra',
  // images: { unoptimized: true },
  // trailingSlash: true,

  webpack: (config, { isServer }) => {
    // Ensure proper module resolution
    config.resolve = config.resolve || {};
    config.resolve.extensionAlias = {
      '.js': ['.js', '.ts', '.tsx', '.jsx'],
      '.mjs': ['.mjs', '.mts'],
      '.cjs': ['.cjs', '.cts'],
    };

    return config;
  },
};

export default withNextra(nextConfig);

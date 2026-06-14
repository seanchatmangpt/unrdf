/** @type {import('next').NextConfig} */
const nextConfig = {
  // Server mode — enables RSC, route handlers, server actions, streaming Suspense.
  // Removed output:'export' which blocked all server-side Next.js 16 features.

  reactStrictMode: true,

  // PPR: static shell + dynamic streaming islands per route segment
  experimental: {
    ppr: true,
  },

  webpack: (config) => {
    config.resolve.extensionAlias = {
      '.js': ['.js', '.mjs'],
    };
    return config;
  },
};

export default nextConfig;

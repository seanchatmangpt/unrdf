/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,
  transpilePackages: ['unrdf'],
  experimental: {
    esmExternals: 'loose'
  }
};

export default nextConfig;

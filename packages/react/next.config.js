/** @type {import('next').NextConfig} */
const nextConfig = {
  output: 'export',
  basePath: '/unrdf',
  images: {
    unoptimized: true,
  },
  reactStrictMode: true,
  pageExtensions: ['js', 'jsx', 'mjs', 'mjsx', 'ts', 'tsx', 'mts', 'mtsx'],
};

export default nextConfig;

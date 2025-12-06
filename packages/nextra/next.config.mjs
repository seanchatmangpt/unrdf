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

  // GitHub Pages configuration
  output: 'export',
  basePath: process.env.NODE_ENV === 'production' ? '/unrdf' : '',
  assetPrefix: process.env.NODE_ENV === 'production' ? '/unrdf/' : '',
  images: { unoptimized: true },
  trailingSlash: true,
};

export default withNextra(nextConfig);

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
};

export default withNextra(nextConfig);

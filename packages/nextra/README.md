# @unrdf/nextra-docs

UNRDF documentation site built with Nextra 4.6.1 + Next.js 16.

## ⚠️ Current Status: Waiting for Turbopack Fix

**This package is currently not buildable due to a known bug in Nextra 4.6.1 + Next.js 16 + Turbopack.**

**Issue**: Turbopack cannot resolve `next-mdx-import-source-file` in Nextra's internal code
**Tracking**: [Nextra Issue #3517](https://github.com/shuding/nextra/issues/3517)
**Workaround**: Use Docusaurus 3 (in `/apps/docs-site`) until this is fixed

**When Fixed**: This package will provide a Next.js-based documentation alternative with better developer experience.

## Features

- **Nextra 4.6.1**: Latest version with App Router support
- **Next.js 16**: Latest Next.js with Webpack (Turbopack disabled due to MDX bugs)
- **React 19**: Latest React
- **TypeScript 5.9**: Full type safety
- **MDX 3**: Advanced markdown with JSX components
- **Search**: Built-in full-text search
- **LaTeX**: Math equations support
- **Code Highlighting**: Syntax highlighting for all languages

## Development

```bash
# Install dependencies
pnpm install

# Start dev server (port 3003)
pnpm dev

# Build for production
pnpm build

# Preview production build
pnpm start
```

## Structure

```
packages/nextra/
├── app/                    # Next.js App Router
│   ├── layout.tsx         # Root layout
│   ├── page.mdx           # Landing page
│   ├── guides/            # Guides section
│   ├── reference/         # API reference
│   ├── concepts/          # Conceptual docs
│   └── examples/          # Examples
├── components/            # React components
├── public/                # Static assets
├── next.config.mjs        # Next.js + Nextra config
├── theme.config.tsx       # Nextra theme config
└── tsconfig.json          # TypeScript config
```

## Key Configuration

### Webpack vs Turbopack

This package uses **Webpack** instead of Turbopack because:
- Nextra 4.6.1 + Next.js 16 + Turbopack has MDX import bugs
- Webpack is stable and well-tested with Nextra

### Port

Dev server runs on port **3003** (to avoid conflicts with other apps).

## Advantages over Docusaurus

- **Next.js Integration**: Seamless integration with Next.js apps
- **React Server Components**: Modern React patterns
- **Better Performance**: Faster page loads with Next.js optimizations
- **Developer Experience**: Hot reload, better TypeScript support
- **Component Flexibility**: Use any React component in MDX

## Deployment

Can be deployed as:
- **Standalone site**: Vercel, Netlify, etc.
- **Static export**: GitHub Pages (with `output: 'export'`)
- **Next.js server**: Any Node.js hosting

## Learn More

- [Nextra Documentation](https://nextra.site)
- [Next.js Documentation](https://nextjs.org/docs)
- [MDX Documentation](https://mdxjs.com)

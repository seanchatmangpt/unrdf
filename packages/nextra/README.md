# @unrdf/nextra-docs

UNRDF documentation site built with Nextra 4.6.1 + Next.js 16.

## ✅ Status: Working (Webpack Mode)

**This package works with Next.js 16.0.7 using Webpack instead of Turbopack.**

**Solution**: Use `next build --webpack` flag to force Webpack mode
**Reference**: Official Nextra examples use this approach
**Alternative**: Docusaurus 3 (in `/apps/docs-site`) for production deployment

## Features

- **Nextra 4.6.1**: Latest version with App Router support
- **Next.js 16.0.7**: Latest Next.js with Webpack mode (`--webpack` flag)
- **React 19.2.1**: Latest React
- **TypeScript 5.9.3**: Full type safety
- **MDX 3**: Advanced markdown with JSX components
- **Search**: Built-in full-text search (codeblocks disabled for performance)
- **LaTeX**: Math equations support
- **Code Highlighting**: Syntax highlighting with copy button

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

This package uses **Webpack** instead of Turbopack:
- Next.js 16 defaults to Turbopack, but `--webpack` flag forces Webpack mode
- Nextra 4.6.1 works correctly with Webpack (official examples use this approach)
- No theme.config.tsx required in Nextra 4 (configuration moved to next.config.mjs)

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

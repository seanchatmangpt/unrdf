# Known Issues

## Nextra 4.6.1 + Next.js 16 Turbopack Incompatibility

### Problem

Nextra 4.6.1 is incompatible with Next.js 16's Turbopack bundler. The build fails with:

```
Module not found: Can't resolve 'next-mdx-import-source-file'
```

This error occurs in Nextra's own internal code (`nextra/dist/client/setup-page.js:3`), not in user code.

### Root Cause

1. Next.js 16 is **Turbopack-only** - there's no way to use Webpack
2. Turbopack doesn't properly resolve the virtual module `next-mdx-import-source-file` that Nextra expects
3. This is a bug in either Nextra 4's Turbopack integration OR Next.js 16's MDX handling

### Attempted Solutions

❌ **Adding `mdx-components.tsx`** - Doesn't help, error is in Nextra's code
❌ **Setting `experimental.turbo: undefined`** - Next.js 16 ignores this
❌ **Webpack fallback** - Doesn't exist in Next.js 16

### Workaround

Use **Docusaurus 3** instead (available in `/apps/docs-site`):
- ✅ Works with React 19, latest dependencies
- ✅ Successful builds
- ✅ GitHub Pages deployment ready
- ✅ All features working

### Timeline

**When this will be fixed:**
- Option 1: Nextra releases 4.7+ with Turbopack fix
- Option 2: Next.js 16.1+ fixes MDX virtual module resolution
- Option 3: Next.js 17 restores Webpack support

**Estimated**: Q1 2026 (based on typical Next.js release cycles)

### Alternatives

While waiting for the fix, you can:

1. **Use Docusaurus 3** (`/apps/docs-site`) - Recommended, production-ready
2. **Use Nextra 3** with Next.js 15 - Older but stable
3. **Wait for Nextra 4.7+** - Keep this package for future use

### References

- [Nextra 4 Turbopack Issues](https://github.com/shuding/nextra/issues?q=is%3Aissue+is%3Aopen+turbopack)
- [Next.js 16 MDX Support](https://nextjs.org/docs/app/api-reference/file-conventions/mdx-components)
- [Turbopack Roadmap](https://turbo.build/repo/docs/core-concepts/monorepos/running-tasks)

---

**Status**: Blocked until upstream fix | **Last Updated**: December 2025

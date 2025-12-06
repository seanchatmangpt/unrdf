# Nextra 4 + Next.js 16 Solution

## Problem

Nextra 4.6.1 + Next.js 16 with default Turbopack bundler fails with:
```
Module not found: Can't resolve 'next-mdx-import-source-file'
```

## Solution

Use the `--webpack` flag to force Webpack mode instead of Turbopack.

### Implementation

**package.json**:
```json
{
  "scripts": {
    "build": "next build --webpack",
    "dev": "next dev --port 3003"
  }
}
```

**next.config.mjs**:
```javascript
import nextra from 'nextra';

const withNextra = nextra({
  latex: true,
  search: { codeblocks: false },
  defaultShowCopyCode: true,
});

export default withNextra({
  reactStrictMode: true,
});
```

**No theme.config.tsx needed** - Nextra 4 uses App Router configuration instead.

## Evidence

Official Nextra examples use this approach:
- `nextra-repo/examples/docs/package.json` uses `"build": "next build --webpack"`
- Simple next.config.mjs without Turbopack workarounds
- No theme.config.tsx file (optional in Nextra 4)

## Build Output

```
✓ Compiled successfully in 683.5ms
✓ Generating static pages (4/4) in 283.7ms

Route (app)
┌ ○ /
├ ○ /_not-found
└ ○ /guides/getting-started
```

## References

- Official Nextra repository: https://github.com/shuding/nextra
- Example implementation: `vendors/nextra-repo/examples/docs`
- Next.js Webpack docs: https://nextjs.org/docs/app/api-reference/cli/next#next-build-options

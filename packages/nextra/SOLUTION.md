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

**app/layout.tsx** (Required - Theme Components):
```tsx
import { Footer, Layout, Navbar } from 'nextra-theme-docs';
import { Head } from 'nextra/components';
import { getPageMap } from 'nextra/page-map';
import 'nextra-theme-docs/style.css';

export default async function RootLayout({ children }) {
  const navbar = <Navbar logo={<strong>UNRDF</strong>} />;
  const footer = <Footer>MIT {new Date().getFullYear()} © UNRDF</Footer>;

  return (
    <html lang="en" dir="ltr" suppressHydrationWarning>
      <Head />
      <body>
        <Layout
          navbar={navbar}
          footer={footer}
          pageMap={await getPageMap()}
          sidebar={{ autoCollapse: true }}
          darkMode={true}
          docsRepositoryBase="https://github.com/seanchatmangpt/unrdf"
        >
          {children}
        </Layout>
      </body>
    </html>
  );
}
```

**Critical**: The `<Layout>` component from `nextra-theme-docs` is **required** for theme rendering. Without it, you only get unstyled MDX content. Theme configuration is passed as props to `<Layout>`, not via a separate config file.

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

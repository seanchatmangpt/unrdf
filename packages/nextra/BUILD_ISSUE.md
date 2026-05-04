# Build Issue: Static Generation Failing

## Status: 🔴 BLOCKING

**Last Updated**: 2025-12-06

## Error

```
Error occurred prerendering page "/_not-found"
Export encountered an error on /_not-found/page: /_not-found, exiting the build.
Next.js build worker exited with code: 1 and signal: null
```

## What Works

- ✅ TypeScript compilation (36-40s)
- ✅ Webpack compilation
- ✅ Layout component implementation
- ✅ Dev server (with `--webpack` flag)

## What Fails

- ❌ Static page generation for `/_not-found` route
- ❌ Production build (`npm run build`)
- ❌ Static export (`output: 'export'`)

## Root Cause Analysis

The error occurs during static page generation phase, specifically when trying to render the `/_not-found` page. This happens regardless of:

1. Whether `output: 'export'` is enabled or disabled
2. Whether a custom `not-found.tsx` exists
3. Whether `not-found.tsx` is a client component or server component

### Hypothesis

The issue likely stems from:
1. **Async Layout**: `app/layout.tsx` uses `await getPageMap()` which makes it async
2. **Static Generation**: Next.js 16's static generation may not fully support async layouts during the _not-found page generation
3. **Nextra Compatibility**: Potential incompatibility between Nextra 4.6.1's `getPageMap()` and Next.js 16's prerendering

## Evidence

### Successful Compilation
```
✓ Compiled successfully in 36.8s
Running TypeScript ... ✅ PASSED
Collecting page data using 15 workers ...
Generating static pages using 15 workers (0/22) ...
```

### Failure Point
```
Error occurred prerendering page "/_not-found"
digest: '3583697844'
```

## Attempted Fixes

1. ❌ Creating custom `not-found.tsx` - No effect
2. ❌ Making `not-found.tsx` a client component - No effect
3. ❌ Disabling `output: 'export'` - Still fails
4. ❌ Removing `not-found.tsx` - Still fails (Next.js auto-generates it)

## Workarounds

### For Development
```bash
npm run dev  # Works fine with --webpack flag
```

### For GitHub Pages Deployment
Currently blocked. Options:
1. **Fix the async layout issue** (preferred)
2. **Use alternative deployment** (Vercel, Netlify) that doesn't require static export
3. **Downgrade Next.js** to version that supports async layouts in static export
4. **Modify Nextra** to make getPageMap() synchronous (may not be possible)

## Next Steps

1. Research Nextra 4 + Next.js 16 compatibility
2. Check if `getPageMap()` can be made synchronous
3. Investigate alternative layout patterns that avoid async
4. Consider filing issue with Nextra maintainers
5. Test with Next.js 15 or earlier versions

## Related Files

- `app/layout.tsx` - Uses `await getPageMap()`
- `next.config.mjs` - Export configuration
- `package.json` - Build scripts

## Impact

- 🟢 **Development**: No impact (dev server works)
- 🔴 **Production Build**: Completely blocked
- 🔴 **GitHub Pages**: Cannot deploy
- 🟢 **Layout Fix**: Not affected (styling is correct)

## References

- Next.js Prerender Error: https://nextjs.org/docs/messages/prerender-error
- Nextra getPageMap: https://nextra.site/docs/advanced/page-map
- Next.js Static Export: https://nextjs.org/docs/app/building-your-application/deploying/static-exports

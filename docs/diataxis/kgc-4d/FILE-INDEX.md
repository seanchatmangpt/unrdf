# KGC-4D Diataxis Documentation - Complete File Index

**Created:** 2025-12-27  
**Total Files:** 31 markdown files  
**Evidence Base:** kgc-4d v5.0.1 (176/176 tests passing)

---

## All Files (Absolute Paths)

### Main Navigation
```
/home/user/unrdf/docs/diataxis/kgc-4d/README.md
/home/user/unrdf/docs/diataxis/kgc-4d/STRUCTURE-SUMMARY.md
/home/user/unrdf/docs/diataxis/kgc-4d/FILE-INDEX.md (this file)
```

### Tutorials (6 files)
```
/home/user/unrdf/docs/diataxis/kgc-4d/tutorials/README.md
/home/user/unrdf/docs/diataxis/kgc-4d/tutorials/01-nanosecond-timestamps.md
/home/user/unrdf/docs/diataxis/kgc-4d/tutorials/02-create-freeze-universe.md
/home/user/unrdf/docs/diataxis/kgc-4d/tutorials/03-time-travel.md
/home/user/unrdf/docs/diataxis/kgc-4d/tutorials/04-query-event-logs.md
/home/user/unrdf/docs/diataxis/kgc-4d/tutorials/05-vector-clocks.md
```

### How-To Guides (7 files)
```
/home/user/unrdf/docs/diataxis/kgc-4d/how-to/README.md
/home/user/unrdf/docs/diataxis/kgc-4d/how-to/01-freeze-and-verify.md
/home/user/unrdf/docs/diataxis/kgc-4d/how-to/02-implement-time-travel.md
/home/user/unrdf/docs/diataxis/kgc-4d/how-to/03-query-event-history.md
/home/user/unrdf/docs/diataxis/kgc-4d/how-to/04-vector-clocks.md
/home/user/unrdf/docs/diataxis/kgc-4d/how-to/05-optimize-performance.md
/home/user/unrdf/docs/diataxis/kgc-4d/how-to/06-work-with-hdit.md
```

### Reference (9 files)
```
/home/user/unrdf/docs/diataxis/kgc-4d/reference/README.md
/home/user/unrdf/docs/diataxis/kgc-4d/reference/kgc-store-api.md
/home/user/unrdf/docs/diataxis/kgc-4d/reference/git-backbone-api.md
/home/user/unrdf/docs/diataxis/kgc-4d/reference/time-api.md
/home/user/unrdf/docs/diataxis/kgc-4d/reference/vector-clock-api.md
/home/user/unrdf/docs/diataxis/kgc-4d/reference/receipt-schema.md
/home/user/unrdf/docs/diataxis/kgc-4d/reference/events-predicates.md
/home/user/unrdf/docs/diataxis/kgc-4d/reference/named-graphs.md
/home/user/unrdf/docs/diataxis/kgc-4d/reference/hdit-system.md
```

### Explanation (7 files)
```
/home/user/unrdf/docs/diataxis/kgc-4d/explanation/README.md
/home/user/unrdf/docs/diataxis/kgc-4d/explanation/01-why-nanosecond-precision.md
/home/user/unrdf/docs/diataxis/kgc-4d/explanation/02-how-time-travel-works.md
/home/user/unrdf/docs/diataxis/kgc-4d/explanation/03-zero-info-invariant.md
/home/user/unrdf/docs/diataxis/kgc-4d/explanation/04-why-git-backing.md
/home/user/unrdf/docs/diataxis/kgc-4d/explanation/05-why-blake3.md
/home/user/unrdf/docs/diataxis/kgc-4d/explanation/06-monotonic-clocks.md
```

---

## Quick Access

**Start here:**  
/home/user/unrdf/docs/diataxis/kgc-4d/README.md

**For beginners:**  
/home/user/unrdf/docs/diataxis/kgc-4d/tutorials/01-nanosecond-timestamps.md

**For quick tasks:**  
/home/user/unrdf/docs/diataxis/kgc-4d/how-to/01-freeze-and-verify.md

**For API lookup:**  
/home/user/unrdf/docs/diataxis/kgc-4d/reference/kgc-store-api.md

**For architecture understanding:**  
/home/user/unrdf/docs/diataxis/kgc-4d/explanation/01-why-nanosecond-precision.md

---

## Evidence Sources

All documentation references:

**Source Code:**
- /home/user/unrdf/packages/kgc-4d/src/index.mjs (exports)
- /home/user/unrdf/packages/kgc-4d/src/time.mjs (timestamps, vector clocks)
- /home/user/unrdf/packages/kgc-4d/src/freeze.mjs (freeze, time travel)
- /home/user/unrdf/packages/kgc-4d/src/store.mjs (KGCStore)
- /home/user/unrdf/packages/kgc-4d/src/git.mjs (Git backbone)
- /home/user/unrdf/packages/kgc-4d/src/hdit/index.mjs (HDIT)

**Tests:**
- /home/user/unrdf/packages/kgc-4d/test/time.test.mjs
- /home/user/unrdf/packages/kgc-4d/test/freeze.test.mjs
- /home/user/unrdf/packages/kgc-4d/test/store.test.mjs
- /home/user/unrdf/packages/kgc-4d/test/4d-time-travel-validation.test.mjs
- /home/user/unrdf/packages/kgc-4d/test/hdit/ (HDIT tests)

**Examples:**
- /home/user/unrdf/packages/kgc-4d/examples/basic-usage.mjs
- /home/user/unrdf/packages/kgc-4d/examples/mission-critical.mjs
- /home/user/unrdf/packages/kgc-4d/examples/local-first-collaboration.mjs

**Architecture:**
- /home/user/unrdf/packages/kgc-4d/docs/ARD.md (requirements)
- /home/user/unrdf/packages/kgc-4d/README.md (overview)

---

**Status:** ✅ Complete Diataxis structure for KGC-4D package

# How-To: Validate Telemetry Against the Semantic Convention Registry

**Objective:** Use OTel Weaver to verify that spans and metrics emitted by UNRDF conform to the semantic convention registry — catching typos, missing attributes, and unknown attribute names before they reach production.

**Audience:** Developers adding instrumentation or modifying the semantic convention registry.

**Prerequisites:**

- OTel Weaver v0.22.1+ installed (`~/.cargo/bin/weaver` or `weaver` on PATH)
- UNRDF checked out at `~/unrdf/`

**Capability atoms:** `otel/registry/`, `otel/live-check-samples.json`, `@unrdf/otel`

---

## 1. Validate the Registry Itself

```bash
cd ~/unrdf/otel
weaver registry check --registry registry/
```

Expected clean output:

```
ℹ Found registry manifest: registry/manifest.yaml
✔ No `after_resolution` policy violation
```

Any `×` errors indicate malformed YAML in a registry file.

---

## 2. Run the Live Check Against Sample Telemetry

```bash
cd ~/unrdf/otel
weaver registry live-check \
  --registry registry/ \
  --input-source live-check-samples.json \
  --input-format json \
  --no-stream
```

A passing run reports:

```
Advisories:   0
Coverage:     100.0%
✔ Performed live check for registry `registry/`
```

---

## 3. Add a New Span to the Samples

When you add a new span to the daemon or CLI, add a corresponding sample to `otel/live-check-samples.json`:

```json
{
  "span": {
    "name": "unrdf.my.operation",
    "kind": "internal",
    "attributes": [{ "name": "unrdf.my.attr", "value": "example-value" }]
  }
}
```

The attribute name must match an `id:` defined in one of the `otel/registry/*.yaml` files. If weaver reports a `violation` (not_found), add the attribute to the registry first.

---

## 4. Add a New Attribute to the Registry

1. Open the appropriate `otel/registry/*.yaml` file (or create a new one).
2. Add the attribute following this pattern:
   ```yaml
   - id: unrdf.my.new_attr
     type: string
     stability: stable
     brief: One-line description of what this attribute captures.
     examples: ['value-a', 'value-b']
   ```
3. Re-run `weaver registry check` to validate syntax.
4. Re-generate the ESM constants:
   ```bash
   weaver registry generate --registry registry/ js ../packages/otel/src/generated/
   ```
5. Import `ATTR_UNRDF_MY_NEW_ATTR` from `@unrdf/otel/attributes` in your instrumentation code.
6. Add a span sample to `live-check-samples.json` and re-run the live check.

---

## 5. Check Registry Coverage

The live-check summary shows what fraction of registry entities were exercised:

```
Registry coverage
  - entities seen: 100.0%
```

Coverage below 100% means some defined attributes or metrics have no sample. Add samples for the missing entities to ensure the registry stays in sync with what the code actually emits.

---

## Troubleshooting

| Symptom                                                   | Cause                                         | Fix                                                                            |
| --------------------------------------------------------- | --------------------------------------------- | ------------------------------------------------------------------------------ |
| `violation: Attribute 'x' does not exist in the registry` | Attribute emitted by code but not in registry | Add `id: x` to the correct registry YAML                                       |
| `improvement: not stable; stability = development`        | Attribute has `stability: experimental`       | Change to `stability: stable`                                                  |
| `No registry manifest found`                              | Missing `otel/registry/manifest.yaml`         | Ensure file contains `schema_url:`                                             |
| Templates produce empty files                             | Wrong `application_mode` in `weaver.yaml`     | Confirm `application_mode: single` in `otel/templates/registry/js/weaver.yaml` |

---

## Related

- `otel/registry/` — canonical attribute/metric definitions
- `otel/live-check-samples.json` — sample telemetry for all 56 registry entities
- `packages/otel/src/generated/` — generated ESM constants (do not edit)
- `docs/telemetry/OTEL-WEAVER-INTEGRATION.md` — integration architecture

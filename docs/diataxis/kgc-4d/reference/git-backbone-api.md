# Reference: GitBackbone API

**Source:** `/home/user/unrdf/packages/kgc-4d/src/git.mjs`

---

## Class: GitBackbone

Pure JavaScript Git operations (no CLI) using isomorphic-git.

### Constructor

```javascript
new GitBackbone(repoPath: string, options?: object)
```

**Parameters:**
- `repoPath` - Path to Git repository
- `options.fs` - (Optional) File system implementation for browser

---

## Methods

### commitSnapshot()

Commit N-Quads snapshot to Git.

**Signature:**
```javascript
commitSnapshot(nquads: string, message: string): Promise<string>
```

**Returns:** Git commit hash (SHA-1)

---

### readSnapshot()

Read N-Quads snapshot from Git commit.

**Signature:**
```javascript
readSnapshot(commitHash: string): Promise<string>
```

**Returns:** N-Quads content

---

## Related

- [Tutorial 02: Freeze Universe](../tutorials/02-create-freeze-universe.md)
- [Receipt Schema](./receipt-schema.md)

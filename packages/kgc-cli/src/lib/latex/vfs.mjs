/**
 * VFS (Virtual File System) - Main export
 *
 * Re-exports all VFS utilities from the vfs/ submodule.
 * Use this for backward compatibility or import directly from vfs/index.mjs
 *
 * @module lib/latex/vfs
 */

export * from './vfs/index.mjs';

// Alias for backward compatibility
export { packDirectory as collectProjectFiles } from './vfs/pack.mjs';

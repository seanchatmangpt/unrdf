/**
 * @file Sidecar config wrapper module
 * @module src/sidecar/config
 *
 * @description
 * Re-exports sidecar configuration from the actual implementation in sidecar directory.
 * This allows tests to import from the expected src/ path.
 */

import SidecarConfigClass, { SidecarConfig as SidecarConfigExport, createSidecarConfig } from '../../sidecar/sidecar/config.mjs';

export { SidecarConfigExport as SidecarConfig, createSidecarConfig };
export default SidecarConfigClass;

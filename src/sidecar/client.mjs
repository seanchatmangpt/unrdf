/**
 * @file Sidecar client wrapper module
 * @module src/sidecar/client
 *
 * @description
 * Re-exports sidecar client from the actual implementation in sidecar directory.
 * This allows tests to import from the expected src/ path.
 */

import SidecarClientClass, { SidecarClient as SidecarClientExport, createSidecarClient } from '../../sidecar/sidecar/client.mjs';

export { SidecarClientExport as SidecarClient, createSidecarClient };
export default SidecarClientClass;

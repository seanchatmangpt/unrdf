/**
 * Auto-generated from ggen ontology
 * Source: schemas/unrdf-packages.ttl
 * Generated: 2026-01-01T03:08:45.661Z
 */

export interface Package {
  name: string;
  version: string;
  description: string;
  tier: 'essential' | 'extended' | 'optional';
  mainExport: string;
  testCoverage: number;
  label: string;
}

export interface PackageRegistry {
  packages: Package[];
  essential: Package[];
  extended: Package[];
  optional: Package[];
  total: number;
}

export const PACKAGES: Record<string, Package> = {

};

export function getRegistry(): PackageRegistry {
  return {
    packages: Object.values(PACKAGES),
    essential: Object.values(PACKAGES).filter(p => p.tier === 'essential'),
    extended: Object.values(PACKAGES).filter(p => p.tier === 'extended'),
    optional: Object.values(PACKAGES).filter(p => p.tier === 'optional'),
    total: Object.keys(PACKAGES).length
  };
}

export function getPackage(name: string): Package | undefined {
  return PACKAGES[name];
}

export function findByTier(tier: 'essential' | 'extended' | 'optional'): Package[] {
  return Object.values(PACKAGES).filter(p => p.tier === tier);
}

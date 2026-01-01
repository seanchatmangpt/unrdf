export { PackageResolver, default as PackageResolverDefault } from './package-resolver.mjs';
export { DIContainer, default as DIContainerDefault } from './di-container.mjs';
export { MetadataCache, default as MetadataCacheDefault } from './metadata-cache.mjs';
export { RuntimeLoader, default as RuntimeLoaderDefault } from './runtime-loader.mjs';
export { ExecutionEnvironment, default as ExecutionEnvironmentDefault } from './execution-env.mjs';
export { PackageLauncher, default as PackageLauncherDefault } from './package-launcher.mjs';
export { PACKAGES, REGISTRY, getPackage, findByTier, stats } from './index.mjs';

// Factory functions for convenience
export async function createResolver() {
  const { REGISTRY } = await import('./index.mjs');
  const { PackageResolver } = await import('./package-resolver.mjs');
  return new PackageResolver(REGISTRY);
}

export async function createDIContainer() {
  const { REGISTRY } = await import('./index.mjs');
  const { DIContainer } = await import('./di-container.mjs');
  return new DIContainer(REGISTRY);
}

export async function createMetadataCache() {
  const { REGISTRY } = await import('./index.mjs');
  const { MetadataCache } = await import('./metadata-cache.mjs');
  return new MetadataCache(REGISTRY);
}

export async function createRuntimeLoader(projectRoot) {
  const { REGISTRY } = await import('./index.mjs');
  const { RuntimeLoader } = await import('./runtime-loader.mjs');
  return new RuntimeLoader(REGISTRY, projectRoot);
}

export async function createExecutionEnvironment(projectRoot) {
  const { REGISTRY } = await import('./index.mjs');
  const { ExecutionEnvironment } = await import('./execution-env.mjs');
  return new ExecutionEnvironment(REGISTRY, projectRoot);
}

export async function createPackageLauncher() {
  const { REGISTRY } = await import('./index.mjs');
  const { PackageLauncher } = await import('./package-launcher.mjs');
  return new PackageLauncher(REGISTRY);
}

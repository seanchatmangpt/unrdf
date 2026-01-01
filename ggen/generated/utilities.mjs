export { PackageResolver, default as PackageResolverDefault } from './package-resolver.mjs';
export { DIContainer, default as DIContainerDefault } from './di-container.mjs';
export { MetadataCache, default as MetadataCacheDefault } from './metadata-cache.mjs';
export { RuntimeLoader, default as RuntimeLoaderDefault } from './runtime-loader.mjs';
export { ExecutionEnvironment, default as ExecutionEnvironmentDefault } from './execution-env.mjs';
export { PackageLauncher, default as PackageLauncherDefault } from './package-launcher.mjs';
export { AIIntegration, default as AIIntegrationDefault } from './ai-integration.mjs';
export { VectorDB, default as VectorDBDefault } from './vector-db.mjs';
export { RealtimeSync, default as RealtimeSyncDefault } from './realtime-sync.mjs';
export { JobQueue, default as JobQueueDefault } from './job-queue.mjs';
export { PACKAGES, REGISTRY, getPackage, findByTier, stats } from './index.mjs';

// All-in-one initialization
export async function initializeBleedingEdge(projectRoot) {
  const [
    { PackageResolver },
    { DIContainer },
    { MetadataCache },
    { RuntimeLoader },
    { ExecutionEnvironment },
    { PackageLauncher },
    { AIIntegration },
    { VectorDB },
    { RealtimeSync },
    { JobQueue },
    { REGISTRY }
  ] = await Promise.all([
    import('./package-resolver.mjs'),
    import('./di-container.mjs'),
    import('./metadata-cache.mjs'),
    import('./runtime-loader.mjs'),
    import('./execution-env.mjs'),
    import('./package-launcher.mjs'),
    import('./ai-integration.mjs'),
    import('./vector-db.mjs'),
    import('./realtime-sync.mjs'),
    import('./job-queue.mjs'),
    import('./index.mjs').then(m => m.REGISTRY)
  ]);

  return {
    resolver: new PackageResolver(REGISTRY),
    di: new DIContainer(REGISTRY),
    cache: new MetadataCache(REGISTRY),
    loader: new RuntimeLoader(REGISTRY, projectRoot),
    env: new ExecutionEnvironment(REGISTRY, projectRoot),
    launcher: new PackageLauncher(REGISTRY),
    ai: new AIIntegration(),
    vectors: new VectorDB(),
    sync: new RealtimeSync(),
    queue: new JobQueue(),
    registry: REGISTRY
  };
}

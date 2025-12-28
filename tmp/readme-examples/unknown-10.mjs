const core = await createKnowledgeSubstrateCore({
  // Default: all enabled (the 20% that delivers 80% of value)
  enableTransactionManager: true,
  enableKnowledgeHookManager: true,
  enableValidation: true,
  enableObservability: true,

  // Optional: enable only if needed
  enableFederation: false,
  enableStreaming: false,
  enableBrowserSupport: false,
});
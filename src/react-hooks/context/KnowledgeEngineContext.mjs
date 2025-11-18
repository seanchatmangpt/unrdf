/**
 * @fileoverview KnowledgeEngineContext - React context for knowledge engine
 * @module react-hooks/context/KnowledgeEngineContext
 */

import { createContext } from 'react';

/**
 * Context for sharing Knowledge Engine instance across components
 */
export const KnowledgeEngineContext = createContext(null);

export const KnowledgeEngineProvider = KnowledgeEngineContext.Provider;
export const KnowledgeEngineConsumer = KnowledgeEngineContext.Consumer;

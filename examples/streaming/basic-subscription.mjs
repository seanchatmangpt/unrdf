/**
 * @file Basic WebSocket Subscription Example
 * @description
 * Demonstrates how to create and manage WebSocket subscriptions for
 * real-time RDF graph updates.
 */

import {
  createSubscriptionManager,
  SubscriptionPatternType,
} from '../../packages/knowledge-engine/streaming/index.mjs';

async function main() {
  // Create subscription manager
  const manager = createSubscriptionManager({
    url: 'ws://localhost:8080/graph-updates',
    reconnectInterval: 5000,
    maxReconnectAttempts: 10,
    heartbeatInterval: 30000,
  });

  // Connect to WebSocket server
  try {
    await manager.connect();
    console.log('Connected to WebSocket server');
  } catch (error) {
    console.error('Failed to connect:', error.message);
    return;
  }

  // Subscribe to property changes for a specific entity
  const propertySubId = manager.subscribe(
    {
      pattern: SubscriptionPatternType.PROPERTY_CHANGE,
      subject: 'http://example.org/alice',
      predicate: 'http://example.org/age',
      debounceMs: 100,
    },
    data => {
      console.log('Property changed:', data);
    }
  );

  console.log('Subscribed to property changes:', propertySubId);

  // Subscribe to all entity updates
  const entitySubId = manager.subscribe(
    {
      pattern: SubscriptionPatternType.ENTITY_UPDATE,
      subject: 'http://example.org/alice',
    },
    data => {
      console.log('Entity updated:', data);
    }
  );

  console.log('Subscribed to entity updates:', entitySubId);

  // Subscribe using SPARQL SELECT
  const sparqlSubId = manager.subscribe(
    {
      pattern: SubscriptionPatternType.SPARQL_SELECT,
      query: `
      SELECT ?person ?name WHERE {
        ?person a <http://example.org/Person> ;
                <http://example.org/name> ?name .
      }
    `,
    },
    data => {
      console.log('SPARQL results:', data);
    }
  );

  console.log('Subscribed to SPARQL query:', sparqlSubId);

  // Subscribe to all changes (wildcard)
  const wildcardSubId = manager.subscribe(
    {
      pattern: SubscriptionPatternType.WILDCARD,
    },
    data => {
      console.log('Change detected:', data);
    }
  );

  console.log('Subscribed to all changes:', wildcardSubId);

  // Listen for global change events
  manager.on('change', event => {
    console.log('Change event:', {
      subscriptionId: event.subscriptionId,
      pattern: event.pattern,
      timestamp: event.timestamp,
    });
  });

  // Listen for errors
  manager.on('error', error => {
    console.error('Subscription error:', error.message);
  });

  // Listen for connection events
  manager.on('connected', () => {
    console.log('WebSocket connected');
  });

  manager.on('disconnected', () => {
    console.log('WebSocket disconnected');
  });

  // Get metrics
  setInterval(() => {
    const metrics = manager.getMetrics();
    console.log('Metrics:', {
      subscriptions: metrics.subscriptionCount,
      messagesReceived: metrics.messagesReceived,
      messagesSent: metrics.messagesSent,
      avgLatency: metrics.avgLatency.toFixed(2) + 'ms',
      p95Latency: metrics.p95Latency.toFixed(2) + 'ms',
    });
  }, 10000);

  // Handle graceful shutdown
  process.on('SIGINT', async () => {
    console.log('\nShutting down...');

    // Unsubscribe from all subscriptions
    manager.unsubscribeAll();

    // Disconnect
    await manager.disconnect();

    console.log('Cleanup complete');
    process.exit(0);
  });
}

main().catch(console.error);

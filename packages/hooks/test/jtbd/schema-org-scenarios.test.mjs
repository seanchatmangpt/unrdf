/**
 * @vitest-environment node
 * JTBD Tests - Schema.org Ontology Level
 *
 * User expresses intent; system determines all internal transformations.
 * Tests validate outcomes, not mechanisms.
 */

import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { KnowledgeHookManager } from '../../src/hooks/knowledge-hook-manager.mjs';
import { defineHook } from '../../src/hooks/define-hook.mjs';

// Schema.org namespace
const SCHEMA = 'https://schema.org/';

// Helper: Create Schema.org quad
const schemaQuad = (subject, predicate, object, isLiteral = false) => ({
  subject: { termType: 'NamedNode', value: subject },
  predicate: { termType: 'NamedNode', value: `${SCHEMA}${predicate}` },
  object: isLiteral
    ? { termType: 'Literal', value: String(object) }
    : { termType: 'NamedNode', value: object },
  graph: { termType: 'DefaultGraph', value: '' },
});

describe('JTBD: Schema.org Order Scenarios', () => {
  let manager;
  let outcomes;

  beforeEach(() => {
    vi.useFakeTimers();
    outcomes = [];
    manager = new KnowledgeHookManager();
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  describe('JTBD-1: Place order and know if it can be fulfilled', () => {
    it('should accept valid order with available product', async () => {
      // System determines validation rules
      manager.registerHook(defineHook({
        name: 'order-fulfillment',
        trigger: 'before-add',
        validate: (quad) => {
          // Reject orders for discontinued products
          if (quad.predicate.value === `${SCHEMA}orderedItem`) {
            return !quad.object.value.includes('discontinued');
          }
          return true;
        },
      }));

      // User intent: Submit an Order
      const orderQuad = schemaQuad(
        'urn:order:12345',
        'orderedItem',
        'urn:product:widget-active'
      );

      const result = await manager.executeByTrigger('before-add', orderQuad);

      // User sees: "Your order is accepted"
      expect(result.valid).toBe(true);
    });

    it('should reject order for discontinued product', async () => {
      manager.registerHook(defineHook({
        name: 'order-fulfillment',
        trigger: 'before-add',
        validate: (quad) => {
          if (quad.predicate.value === `${SCHEMA}orderedItem`) {
            return !quad.object.value.includes('discontinued');
          }
          return true;
        },
      }));

      // User intent: Submit an Order for discontinued item
      const orderQuad = schemaQuad(
        'urn:order:12346',
        'orderedItem',
        'urn:product:widget-discontinued'
      );

      const result = await manager.executeByTrigger('before-add', orderQuad);

      // User sees: "Your order cannot be fulfilled"
      expect(result.valid).toBe(false);
    });
  });

  describe('JTBD-2: Recurring purchase without intervention', () => {
    it('should execute recurring order at scheduled intervals', async () => {
      const executedOrders = [];

      manager.registerHook(defineHook({
        name: 'recurring-order',
        trigger: 'on-interval',
        validate: () => {
          executedOrders.push(Date.now());
          return true;
        },
      }));

      const recurringOrderQuad = schemaQuad(
        'urn:order:recurring-monthly',
        'orderStatus',
        `${SCHEMA}OrderProcessing`
      );

      // User intent: Set up recurrence (system handles timing)
      // Simulate 3 monthly intervals (30 days each)
      const MONTH_MS = 30 * 24 * 60 * 60 * 1000;

      await manager.executeByTrigger('on-interval', recurringOrderQuad);
      vi.advanceTimersByTime(MONTH_MS);
      await manager.executeByTrigger('on-interval', recurringOrderQuad);
      vi.advanceTimersByTime(MONTH_MS);
      await manager.executeByTrigger('on-interval', recurringOrderQuad);

      // User sees: continuity - 3 orders processed
      expect(executedOrders.length).toBe(3);
      expect(executedOrders[1] - executedOrders[0]).toBe(MONTH_MS);
    });

    it('should handle subscription with price change notification', async () => {
      const notifications = [];
      let currentPrice = 9.99;

      manager.registerHook(defineHook({
        name: 'price-monitor',
        trigger: 'on-interval',
        transform: (quad) => {
          // System monitors pricing changes
          if (currentPrice !== 9.99) {
            notifications.push({
              time: Date.now(),
              message: `Price changed to ${currentPrice}`,
            });
          }
          return quad;
        },
      }));

      const subscriptionQuad = schemaQuad(
        'urn:subscription:premium',
        'price',
        '9.99',
        true
      );

      // Week 1: Normal
      await manager.executeByTrigger('on-interval', subscriptionQuad);
      expect(notifications.length).toBe(0);

      // Week 2: Price increases
      vi.advanceTimersByTime(7 * 24 * 60 * 60 * 1000);
      currentPrice = 12.99;
      await manager.executeByTrigger('on-interval', subscriptionQuad);

      // User notified only when necessary
      expect(notifications.length).toBe(1);
      expect(notifications[0].message).toContain('12.99');
    });
  });

  describe('JTBD-3: Publish product listing with compliance check', () => {
    it('should accept compliant offer', async () => {
      manager.registerHook(defineHook({
        name: 'offer-compliance',
        trigger: 'before-add',
        validate: (quad) => {
          // System determines compliance rules
          if (quad.predicate.value === `${SCHEMA}price`) {
            const price = parseFloat(quad.object.value);
            return price > 0 && price < 1000000;
          }
          return true;
        },
      }));

      const offerQuad = schemaQuad('urn:offer:widget-sale', 'price', '29.99', true);
      const result = await manager.executeByTrigger('before-add', offerQuad);

      // User sees: "Approved"
      expect(result.valid).toBe(true);
    });

    it('should reject offer with invalid price', async () => {
      manager.registerHook(defineHook({
        name: 'offer-compliance',
        trigger: 'before-add',
        validate: (quad) => {
          if (quad.predicate.value === `${SCHEMA}price`) {
            const price = parseFloat(quad.object.value);
            return price > 0 && price < 1000000;
          }
          return true;
        },
      }));

      const offerQuad = schemaQuad('urn:offer:invalid', 'price', '-5.00', true);
      const result = await manager.executeByTrigger('before-add', offerQuad);

      // User sees: "Needs correction"
      expect(result.valid).toBe(false);
    });
  });

  describe('JTBD-5: Shipping address validation', () => {
    it('should accept address in serviced region', async () => {
      const servicedRegions = ['US', 'CA', 'UK', 'DE'];

      manager.registerHook(defineHook({
        name: 'address-validation',
        trigger: 'before-add',
        validate: (quad) => {
          if (quad.predicate.value === `${SCHEMA}addressCountry`) {
            return servicedRegions.includes(quad.object.value);
          }
          return true;
        },
      }));

      const addressQuad = schemaQuad('urn:address:home', 'addressCountry', 'US');
      const result = await manager.executeByTrigger('before-add', addressQuad);

      expect(result.valid).toBe(true);
    });

    it('should reject address outside service area', async () => {
      const servicedRegions = ['US', 'CA', 'UK', 'DE'];

      manager.registerHook(defineHook({
        name: 'address-validation',
        trigger: 'before-add',
        validate: (quad) => {
          if (quad.predicate.value === `${SCHEMA}addressCountry`) {
            return servicedRegions.includes(quad.object.value);
          }
          return true;
        },
      }));

      const addressQuad = schemaQuad('urn:address:intl', 'addressCountry', 'XX');
      const result = await manager.executeByTrigger('before-add', addressQuad);

      expect(result.valid).toBe(false);
    });
  });

  describe('JTBD-6: Bulk product updates', () => {
    it('should process batch updates and report summary', async () => {
      const processedUpdates = [];

      manager.registerHook(defineHook({
        name: 'bulk-processor',
        trigger: 'before-add',
        transform: (quad) => {
          processedUpdates.push(quad.subject.value);
          return quad;
        },
      }));

      // User provides collection of Product modifications
      const products = [
        schemaQuad('urn:product:A', 'price', '10.00', true),
        schemaQuad('urn:product:B', 'price', '20.00', true),
        schemaQuad('urn:product:C', 'price', '30.00', true),
      ];

      for (const quad of products) {
        await manager.executeByTrigger('before-add', quad);
      }

      // User sees summarized outcome
      expect(processedUpdates).toEqual([
        'urn:product:A',
        'urn:product:B',
        'urn:product:C',
      ]);
    });
  });

  describe('JTBD-7: Notification on order-affecting changes', () => {
    it('should notify only when drift exceeds threshold', async () => {
      const notifications = [];
      let inventoryLevel = 100;
      const LOW_STOCK_THRESHOLD = 10;

      manager.registerHook(defineHook({
        name: 'inventory-observer',
        trigger: 'on-interval',
        validate: () => {
          if (inventoryLevel <= LOW_STOCK_THRESHOLD) {
            notifications.push({
              time: Date.now(),
              level: inventoryLevel,
            });
          }
          return true;
        },
      }));

      const inventoryQuad = schemaQuad(
        'urn:product:popular',
        'inventoryLevel',
        String(inventoryLevel),
        true
      );

      // Day 1: Adequate stock (no notification)
      await manager.executeByTrigger('on-interval', inventoryQuad);
      expect(notifications.length).toBe(0);

      // Day 2: Stock depleting (no notification yet)
      vi.advanceTimersByTime(24 * 60 * 60 * 1000);
      inventoryLevel = 50;
      await manager.executeByTrigger('on-interval', inventoryQuad);
      expect(notifications.length).toBe(0);

      // Day 3: Low stock (notification triggered)
      vi.advanceTimersByTime(24 * 60 * 60 * 1000);
      inventoryLevel = 5;
      await manager.executeByTrigger('on-interval', inventoryQuad);

      // User perceives system vigilance
      expect(notifications.length).toBe(1);
      expect(notifications[0].level).toBe(5);
    });
  });

  describe('JTBD-8: Account info consistency', () => {
    it('should propagate profile update across related entities', async () => {
      const entityStates = new Map();

      manager.registerHook(defineHook({
        name: 'profile-sync',
        trigger: 'after-add',
        transform: (quad) => {
          // System reconciles dependencies
          if (quad.predicate.value === `${SCHEMA}email`) {
            entityStates.set('person', quad.object.value);
            entityStates.set('orders', quad.object.value);
            entityStates.set('shipments', quad.object.value);
          }
          return quad;
        },
      }));

      // User updates profile once
      const profileQuad = schemaQuad(
        'urn:person:alice',
        'email',
        'alice@newdomain.com',
        true
      );

      await manager.executeByTrigger('after-add', profileQuad);

      // User sees unified profile (consistency across all entities)
      expect(entityStates.get('person')).toBe('alice@newdomain.com');
      expect(entityStates.get('orders')).toBe('alice@newdomain.com');
      expect(entityStates.get('shipments')).toBe('alice@newdomain.com');
    });
  });
});

describe('JTBD: Time-Sensitive Scenarios', () => {
  let manager;

  beforeEach(() => {
    vi.useFakeTimers();
    vi.setSystemTime(new Date('2025-01-01T00:00:00Z'));
    manager = new KnowledgeHookManager();
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it('should expire offer after validity period', async () => {
    const OFFER_VALID_DAYS = 7;
    const offerCreatedAt = Date.now();

    manager.registerHook(defineHook({
      name: 'offer-expiry',
      trigger: 'before-query',
      validate: () => {
        const daysPassed = (Date.now() - offerCreatedAt) / (24 * 60 * 60 * 1000);
        return daysPassed <= OFFER_VALID_DAYS;
      },
    }));

    const offerQuad = schemaQuad('urn:offer:flash-sale', 'validThrough', '2025-01-08');

    // Day 1: Offer valid
    let result = await manager.executeByTrigger('before-query', offerQuad);
    expect(result.valid).toBe(true);

    // Day 5: Still valid
    vi.advanceTimersByTime(5 * 24 * 60 * 60 * 1000);
    result = await manager.executeByTrigger('before-query', offerQuad);
    expect(result.valid).toBe(true);

    // Day 10: Expired
    vi.advanceTimersByTime(5 * 24 * 60 * 60 * 1000);
    result = await manager.executeByTrigger('before-query', offerQuad);
    expect(result.valid).toBe(false);
  });

  it('should enforce order cutoff time for same-day delivery', async () => {
    manager.registerHook(defineHook({
      name: 'delivery-cutoff',
      trigger: 'before-add',
      validate: () => {
        const hour = new Date().getUTCHours();
        // Same-day delivery cutoff: 14:00 UTC
        return hour < 14;
      },
    }));

    const orderQuad = schemaQuad(
      'urn:order:same-day',
      'deliveryMethod',
      `${SCHEMA}SameDayDelivery`
    );

    // 10:00 UTC: Order accepted
    vi.setSystemTime(new Date('2025-01-01T10:00:00Z'));
    let result = await manager.executeByTrigger('before-add', orderQuad);
    expect(result.valid).toBe(true);

    // 15:00 UTC: Order rejected (past cutoff)
    vi.setSystemTime(new Date('2025-01-01T15:00:00Z'));
    result = await manager.executeByTrigger('before-add', orderQuad);
    expect(result.valid).toBe(false);
  });

  it('should apply time-based pricing tiers', async () => {
    let appliedDiscount = 0;

    manager.registerHook(defineHook({
      name: 'time-pricing',
      trigger: 'before-add',
      transform: (quad) => {
        const hour = new Date().getUTCHours();
        // Happy hour: 17:00-19:00 UTC = 20% off
        if (hour >= 17 && hour < 19) {
          appliedDiscount = 0.20;
        } else {
          appliedDiscount = 0;
        }
        return quad;
      },
    }));

    const orderQuad = schemaQuad('urn:order:dinner', 'orderDate', '2025-01-01');

    // 12:00 UTC: No discount
    vi.setSystemTime(new Date('2025-01-01T12:00:00Z'));
    await manager.executeByTrigger('before-add', orderQuad);
    expect(appliedDiscount).toBe(0);

    // 18:00 UTC: Happy hour discount
    vi.setSystemTime(new Date('2025-01-01T18:00:00Z'));
    await manager.executeByTrigger('before-add', orderQuad);
    expect(appliedDiscount).toBe(0.20);
  });
});

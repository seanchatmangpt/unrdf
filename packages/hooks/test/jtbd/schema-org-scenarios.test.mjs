/**
 * @vitest-environment node
 *
 * JTBD Scenarios - Schema.org Ontology Level
 *
 * User expresses intent; the calculus (μ, Λ, Π, Σ, Q) determines
 * all internal transformations needed to satisfy the job.
 *
 * Users do not choose or interact with mechanisms.
 * They interact only with meaning and outcomes.
 */

import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { KnowledgeHookManager } from '../../src/hooks/knowledge-hook-manager.mjs';
import { defineHook } from '../../src/hooks/define-hook.mjs';

const SCHEMA = 'https://schema.org/';

const quad = (subject, predicate, object, isLiteral = false) => ({
  subject: { termType: 'NamedNode', value: subject },
  predicate: { termType: 'NamedNode', value: `${SCHEMA}${predicate}` },
  object: isLiteral
    ? { termType: 'Literal', value: String(object) }
    : { termType: 'NamedNode', value: object },
  graph: { termType: 'DefaultGraph', value: '' },
});

/**
 * Configure system with 8 internal operations for a scenario.
 * The user never sees these - they see only outcomes.
 */
function configureSystem(manager, operations) {
  operations.forEach((op, i) => {
    manager.registerHook(
      defineHook({
        name: `μ-${i + 1}`,
        trigger: op.trigger || 'before-add',
        validate: op.validate,
        transform: op.transform,
      })
    );
  });
}

describe('JTBD-1: Place order and know if it can be fulfilled', () => {
  let manager;

  beforeEach(() => {
    manager = new KnowledgeHookManager();

    // System determines 8 internal operations (opaque to user)
    configureSystem(manager, [
      { validate: q => q.subject.termType === 'NamedNode' }, // μ₁: Subject coherence
      { validate: q => q.predicate.value.includes('schema.org') }, // μ₂: Ontology membership
      { validate: q => !q.object.value.includes('discontinued') }, // μ₃: Product availability
      { validate: q => !q.object.value.includes('restricted') }, // μ₄: Regional constraint
      { validate: () => true }, // μ₅: Seller verification
      { validate: () => true }, // μ₆: Payment compatibility
      { validate: () => true }, // μ₇: Terms acceptance
      { transform: q => q }, // μ₈: Order finalization
    ]);
  });

  it('User submits order → sees "Your order is accepted"', async () => {
    const order = quad('urn:order:12345', 'orderedItem', 'urn:product:widget-active');
    const outcome = await manager.executeByTrigger('before-add', order);
    expect(outcome.valid).toBe(true);
  });

  it('User submits order for discontinued item → sees "Cannot be fulfilled"', async () => {
    const order = quad('urn:order:12346', 'orderedItem', 'urn:product:widget-discontinued');
    const outcome = await manager.executeByTrigger('before-add', order);
    expect(outcome.valid).toBe(false);
  });
});

describe('JTBD-2: Recurring purchase without intervention', () => {
  let manager;
  let executionLog;
  let currentPrice;

  beforeEach(() => {
    vi.useFakeTimers();
    manager = new KnowledgeHookManager();
    executionLog = [];
    currentPrice = 9.99;

    configureSystem(manager, [
      {
        trigger: 'on-interval',
        validate: () => {
          executionLog.push({ type: 'availability' });
          return true;
        },
      },
      {
        trigger: 'on-interval',
        validate: () => {
          executionLog.push({ type: 'pricing' });
          return true;
        },
      },
      {
        trigger: 'on-interval',
        validate: () => {
          executionLog.push({ type: 'payment' });
          return true;
        },
      },
      {
        trigger: 'on-interval',
        validate: () => {
          executionLog.push({ type: 'address' });
          return true;
        },
      },
      {
        trigger: 'on-interval',
        validate: () => {
          executionLog.push({ type: 'schedule' });
          return true;
        },
      },
      {
        trigger: 'on-interval',
        validate: () => {
          executionLog.push({ type: 'inventory' });
          return true;
        },
      },
      {
        trigger: 'on-interval',
        validate: () => {
          executionLog.push({ type: 'shipping' });
          return true;
        },
      },
      {
        trigger: 'on-interval',
        transform: q => {
          executionLog.push({ type: 'order-created', price: currentPrice });
          return q;
        },
      },
    ]);
  });

  afterEach(() => vi.useRealTimers());

  it('User sets recurrence → system maintains continuity automatically', async () => {
    const subscription = quad('urn:subscription:monthly', 'orderStatus', 'active', true);
    const MONTH = 30 * 24 * 60 * 60 * 1000;

    await manager.executeByTrigger('on-interval', subscription);
    vi.advanceTimersByTime(MONTH);
    await manager.executeByTrigger('on-interval', subscription);
    vi.advanceTimersByTime(MONTH);
    await manager.executeByTrigger('on-interval', subscription);

    // User sees: 3 orders processed (continuity)
    const orders = executionLog.filter(e => e.type === 'order-created');
    expect(orders.length).toBe(3);
  });

  it('User receives notification only when intervention needed', async () => {
    const subscription = quad('urn:subscription:monthly', 'price', '9.99', true);

    await manager.executeByTrigger('on-interval', subscription);
    const initialOrders = executionLog.filter(e => e.type === 'order-created');
    expect(initialOrders[0].price).toBe(9.99);

    currentPrice = 14.99; // Price changed
    await manager.executeByTrigger('on-interval', subscription);

    const updatedOrders = executionLog.filter(e => e.type === 'order-created');
    expect(updatedOrders[1].price).toBe(14.99);
  });
});

describe('JTBD-3: Publish listing and know if it meets requirements', () => {
  let manager;

  beforeEach(() => {
    manager = new KnowledgeHookManager();

    configureSystem(manager, [
      { validate: q => q.subject.termType === 'NamedNode' }, // μ₁: Identifier format
      {
        validate: q => {
          const p = parseFloat(q.object.value);
          return !isNaN(p) && p > 0;
        },
      }, // μ₂: Price validity
      { validate: q => parseFloat(q.object.value) < 1000000 }, // μ₃: Price ceiling
      { validate: () => true }, // μ₄: Category membership
      { validate: () => true }, // μ₅: Seller authorization
      { validate: () => true }, // μ₆: Description completeness
      { validate: () => true }, // μ₇: Image requirements
      { transform: q => q }, // μ₈: Listing activation
    ]);
  });

  it('User inputs compliant offer → sees "Approved"', async () => {
    const offer = quad('urn:offer:widget-sale', 'price', '29.99', true);
    const outcome = await manager.executeByTrigger('before-add', offer);
    expect(outcome.valid).toBe(true);
  });

  it('User inputs invalid price → sees "Needs correction"', async () => {
    const offer = quad('urn:offer:invalid', 'price', '-5.00', true);
    const outcome = await manager.executeByTrigger('before-add', offer);
    expect(outcome.valid).toBe(false);
  });
});

describe('JTBD-4: Payment verified without friction', () => {
  let manager;

  beforeEach(() => {
    manager = new KnowledgeHookManager();

    configureSystem(manager, [
      { validate: q => q.subject.value.startsWith('urn:payment:') }, // μ₁: Payment identifier
      { validate: q => !q.object.value.includes('expired') }, // μ₂: Expiration check
      { validate: q => !q.object.value.includes('blocked') }, // μ₃: Fraud screening
      { validate: () => true }, // μ₄: Issuer verification
      { validate: () => true }, // μ₅: Billing address match
      { validate: () => true }, // μ₆: Spending limit
      { validate: () => true }, // μ₇: Currency compatibility
      { transform: q => q }, // μ₈: Payment authorization
    ]);
  });

  it('User provides valid payment → sees "Payment accepted"', async () => {
    const payment = quad('urn:payment:visa-1234', 'paymentStatus', 'active', true);
    const outcome = await manager.executeByTrigger('before-add', payment);
    expect(outcome.valid).toBe(true);
  });

  it('User provides expired card → sees "Payment requires update"', async () => {
    const payment = quad('urn:payment:visa-expired', 'paymentStatus', 'expired', true);
    const outcome = await manager.executeByTrigger('before-add', payment);
    expect(outcome.valid).toBe(false);
  });
});

describe('JTBD-5: Shipping address works for order', () => {
  let manager;
  const servicedRegions = ['US', 'CA', 'UK', 'DE', 'FR', 'JP', 'AU'];

  beforeEach(() => {
    manager = new KnowledgeHookManager();

    configureSystem(manager, [
      { validate: q => q.subject.termType === 'NamedNode' }, // μ₁: Address identifier
      { validate: q => servicedRegions.includes(q.object.value) }, // μ₂: Region availability
      { validate: () => true }, // μ₃: Postal code format
      { validate: () => true }, // μ₄: Street address completeness
      { validate: () => true }, // μ₅: Logistics feasibility
      { validate: () => true }, // μ₆: Carrier availability
      { validate: () => true }, // μ₇: Delivery estimate
      { transform: q => q }, // μ₈: Address confirmation
    ]);
  });

  it('User provides serviceable address → sees confirmation', async () => {
    const address = quad('urn:address:home', 'addressCountry', 'US', true);
    const outcome = await manager.executeByTrigger('before-add', address);
    expect(outcome.valid).toBe(true);
  });

  it('User provides unserviceable address → sees correction guidance', async () => {
    const address = quad('urn:address:intl', 'addressCountry', 'XX', true);
    const outcome = await manager.executeByTrigger('before-add', address);
    expect(outcome.valid).toBe(false);
  });
});

describe('JTBD-6: Bulk updates processed correctly', () => {
  let manager;
  let processedEntities;

  beforeEach(() => {
    manager = new KnowledgeHookManager();
    processedEntities = [];

    configureSystem(manager, [
      { validate: q => q.subject.termType === 'NamedNode' }, // μ₁: Entity identifier
      { validate: q => q.predicate.value.includes('schema.org') }, // μ₂: Ontology coherence
      { validate: () => true }, // μ₃: Category consistency
      { validate: () => true }, // μ₄: Price range validity
      { validate: () => true }, // μ₅: Availability status
      { validate: () => true }, // μ₆: Description format
      { validate: () => true }, // μ₇: Cross-reference integrity
      {
        transform: q => {
          processedEntities.push(q.subject.value);
          return q;
        },
      }, // μ₈: Batch commit
    ]);
  });

  it('User submits bulk collection → sees summarized outcome', async () => {
    const products = [
      quad('urn:product:A', 'price', '10.00', true),
      quad('urn:product:B', 'price', '20.00', true),
      quad('urn:product:C', 'price', '30.00', true),
      quad('urn:product:D', 'price', '40.00', true),
    ];

    for (const p of products) {
      await manager.executeByTrigger('before-add', p);
    }

    expect(processedEntities).toEqual([
      'urn:product:A',
      'urn:product:B',
      'urn:product:C',
      'urn:product:D',
    ]);
  });
});

describe('JTBD-7: Notification on order-affecting changes', () => {
  let manager;
  let notifications;
  let inventoryLevel;

  beforeEach(() => {
    vi.useFakeTimers();
    manager = new KnowledgeHookManager();
    notifications = [];
    inventoryLevel = 100;

    const THRESHOLD = 10;

    configureSystem(manager, [
      { trigger: 'on-interval', validate: () => true }, // μ₁: Inventory monitor
      { trigger: 'on-interval', validate: () => true }, // μ₂: Price monitor
      { trigger: 'on-interval', validate: () => true }, // μ₃: Availability monitor
      { trigger: 'on-interval', validate: () => true }, // μ₄: Supplier status
      { trigger: 'on-interval', validate: () => true }, // μ₅: Shipping constraints
      { trigger: 'on-interval', validate: () => true }, // μ₆: Regional changes
      {
        trigger: 'on-interval',
        transform: q => {
          if (inventoryLevel <= THRESHOLD) notifications.push({ level: inventoryLevel });
          return q;
        },
      }, // μ₇: Drift detection + notification
      { trigger: 'on-interval', transform: q => q }, // μ₈: Commit
    ]);
  });

  afterEach(() => vi.useRealTimers());

  it('User perceives system vigilance without manual monitoring', async () => {
    const product = quad('urn:product:popular', 'inventoryLevel', '100', true);
    const DAY = 24 * 60 * 60 * 1000;

    // Day 1: Adequate stock
    await manager.executeByTrigger('on-interval', product);
    expect(notifications.length).toBe(0);

    // Day 2: Stock depleting
    vi.advanceTimersByTime(DAY);
    inventoryLevel = 50;
    await manager.executeByTrigger('on-interval', product);
    expect(notifications.length).toBe(0);

    // Day 3: Low stock - user notified
    vi.advanceTimersByTime(DAY);
    inventoryLevel = 5;
    await manager.executeByTrigger('on-interval', product);
    expect(notifications.length).toBe(1);
    expect(notifications[0].level).toBe(5);
  });
});

describe('JTBD-8: Account info aligns everywhere automatically', () => {
  let manager;
  let entityStates;

  beforeEach(() => {
    manager = new KnowledgeHookManager();
    entityStates = new Map();

    configureSystem(manager, [
      { trigger: 'after-add', validate: () => true }, // μ₁: Profile validation
      { trigger: 'after-add', validate: () => true }, // μ₂: Format normalization
      {
        trigger: 'after-add',
        transform: q => {
          entityStates.set('person', q.object.value);
          return q;
        },
      }, // μ₃: Person sync
      {
        trigger: 'after-add',
        transform: q => {
          entityStates.set('orders', q.object.value);
          return q;
        },
      }, // μ₄: Orders sync
      {
        trigger: 'after-add',
        transform: q => {
          entityStates.set('shipments', q.object.value);
          return q;
        },
      }, // μ₅: Shipments sync
      {
        trigger: 'after-add',
        transform: q => {
          entityStates.set('invoices', q.object.value);
          return q;
        },
      }, // μ₆: Invoices sync
      {
        trigger: 'after-add',
        transform: q => {
          entityStates.set('subscriptions', q.object.value);
          return q;
        },
      }, // μ₇: Subscriptions sync
      { trigger: 'after-add', transform: q => q }, // μ₈: Commit
    ]);
  });

  it('User updates profile once → sees unified state everywhere', async () => {
    const profile = quad('urn:person:alice', 'email', 'alice@newdomain.com', true);
    await manager.executeByTrigger('after-add', profile);

    expect(entityStates.get('person')).toBe('alice@newdomain.com');
    expect(entityStates.get('orders')).toBe('alice@newdomain.com');
    expect(entityStates.get('shipments')).toBe('alice@newdomain.com');
    expect(entityStates.get('invoices')).toBe('alice@newdomain.com');
    expect(entityStates.get('subscriptions')).toBe('alice@newdomain.com');
  });
});

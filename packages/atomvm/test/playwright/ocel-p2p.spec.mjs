import { test, expect } from '@playwright/test';

async function waitForStanding(page, field, standing = 'ALIVE') {
  await expect.poll(async () => page.evaluate(({ field }) => window.__atomvmExplorer?.state()?.[field], { field }), {
    timeout: 30000,
    message: `${field} should become ${standing}`,
  }).toBe(standing);
}

function attr(object, name) {
  return object.attributes?.filter(item => item.name === name).at(-1)?.value;
}

test.describe('UNRDF AtomVM OCEL v2 peer explorer', () => {
  test('two real AtomVM/WASM peers exchange one message and converge the OCEL log', async ({ browser }) => {
    const context = await browser.newContext();
    const alice = await context.newPage();
    const bob = await context.newPage();
    const pageErrors = [];
    for (const [name, page] of [['alice', alice], ['bob', bob]]) {
      page.on('pageerror', error => pageErrors.push(`${name}:${error.message}`));
    }

    await Promise.all([
      alice.goto('/?peer=alice', { waitUntil: 'domcontentloaded' }),
      bob.goto('/?peer=bob', { waitUntil: 'domcontentloaded' }),
    ]);
    await Promise.all([
      waitForStanding(alice, 'runtimeStanding'),
      waitForStanding(bob, 'runtimeStanding'),
    ]);

    const runtimeProof = await Promise.all([
      alice.evaluate(async () => window.__atomvmExplorer.runtime.observe('browser-proof')),
      bob.evaluate(async () => window.__atomvmExplorer.runtime.observe('browser-proof')),
    ]);
    expect(runtimeProof[0].checksum).toBe(runtimeProof[1].checksum);
    expect(runtimeProof[0].sequence).toBeGreaterThan(0);
    expect(runtimeProof[1].sequence).toBeGreaterThan(0);

    const offer = await alice.evaluate(() => window.__atomvmExplorer.createOffer());
    const answer = await bob.evaluate(offerText => window.__atomvmExplorer.acceptOffer(offerText), offer);
    await alice.evaluate(answerText => window.__atomvmExplorer.acceptAnswer(answerText), answer);

    await Promise.all([
      waitForStanding(alice, 'p2pStanding'),
      waitForStanding(bob, 'p2pStanding'),
    ]);

    const receipt = await alice.evaluate(() => window.__atomvmExplorer.send('OCEL v2 from AtomVM Alice to AtomVM Bob'));
    expect(receipt.status).toBe('ALIVE');
    expect(receipt.sourceReceipt.checksum).toBe(receipt.targetReceipt.checksum);
    expect(receipt.sourceReceipt.sequence).toBeGreaterThan(0);
    expect(receipt.targetReceipt.sequence).toBeGreaterThan(0);

    await expect.poll(async () => bob.evaluate(() => window.__atomvmExplorer.snapshot().events.some(event => event.type === 'message.acknowledged')), {
      timeout: 10000,
      message: 'Bob should receive the final OCEL acknowledgement projection',
    }).toBe(true);

    const [aliceState, bobState] = await Promise.all([
      alice.evaluate(() => window.__atomvmExplorer.state()),
      bob.evaluate(() => window.__atomvmExplorer.state()),
    ]);

    expect(aliceState.link.channelState).toBe('open');
    expect(bobState.link.channelState).toBe('open');
    expect(aliceState.link.remotePeerId).toBe('bob');
    expect(bobState.link.remotePeerId).toBe('alice');
    expect(aliceState.link.sessionId).toBe(bobState.link.sessionId);

    for (const state of [aliceState, bobState]) {
      const eventTypes = state.ocel.events.map(event => event.type);
      expect(eventTypes).toContain('peer.connected');
      expect(eventTypes).toContain('message.sent');
      expect(eventTypes).toContain('message.received');
      expect(eventTypes).toContain('message.acknowledged');

      const runtimes = state.ocel.objects.filter(object => object.type === 'atomvm-runtime');
      expect(runtimes.map(object => attr(object, 'peerId')).sort()).toEqual(['alice', 'bob']);
      expect(runtimes.every(object => attr(object, 'standing') === 'ALIVE')).toBe(true);

      const messages = state.ocel.objects.filter(object => object.type === 'message');
      expect(messages).toHaveLength(1);
      expect(attr(messages[0], 'sourceChecksum')).toBe(attr(messages[0], 'targetChecksum'));
      expect(attr(messages[0], 'verified')).toBe(true);

      const receive = state.ocel.events.find(event => event.type === 'message.received');
      const verified = receive.attributes.find(item => item.name === 'verified')?.value;
      expect(verified).toBe(true);
      expect(receive.relationships.some(rel => rel.qualifier === 'source-runtime' && rel.objectId === 'atomvm:alice')).toBe(true);
      expect(receive.relationships.some(rel => rel.qualifier === 'target-runtime' && rel.objectId === 'atomvm:bob')).toBe(true);
    }

    await expect(alice.locator('#ocelGraph [data-event-type="message-sent"]')).toHaveCount(0);
    await expect(alice.locator('#ocelGraph [data-event-type="message.sent"]')).toBeVisible();
    await expect(alice.locator('#ocelGraph [data-event-type="message.received"]')).toBeVisible();
    await expect(alice.locator('#ocelGraph [data-event-type="message.acknowledged"]')).toBeVisible();
    expect(pageErrors).toEqual([]);

    await context.close();
  });
});

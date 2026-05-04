/**
 * Deterministic Receipt Service
 */
import { generateHybridKeyPair, signHybrid } from '@unrdf/receipts';

export class ReceiptGenerator {
  constructor() {
    this.keyPair = null;
  }

  async initialize() {
    this.keyPair = await generateHybridKeyPair();
  }

  async generateExecutionReceipt(context, agentId, resultState) {
    if (!this.keyPair) {
      await this.initialize();
    }

    const payload = {
      type: 'PROV-O:Activity',
      agent: agentId,
      used: context,
      generated: resultState,
      timestamp: Date.now()
    };

    const payloadStr = JSON.stringify(payload);
    
    // Create cryptographic signature
    const signature = await signHybrid(payloadStr, this.keyPair.privateKey);

    return {
      payload,
      signature: Buffer.from(signature).toString('base64')
    };
  }
}

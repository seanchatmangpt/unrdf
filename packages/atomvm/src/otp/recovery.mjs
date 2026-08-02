import { Result } from './result.mjs';
import { Proc } from './proc.mjs';
import { cloneInitial, OtpRefusal } from './values.mjs';

export const ProcLib = Object.freeze({
  async startLink(initialState, initHandler, loopHandler, timeoutMs = 5000) {
    if (typeof initHandler !== 'function' || typeof loopHandler !== 'function') {
      throw new TypeError('startLink requires init and loop handlers');
    }
    let acked = false;
    let acknowledge;
    const ack = new Promise(resolve => { acknowledge = resolve; });
    const initAck = () => {
      if (!acked) {
        acked = true;
        acknowledge();
      }
    };

    const withTimeout = (promise, code, message) => new Promise((resolve, reject) => {
      const timer = setTimeout(() => reject(new OtpRefusal(code, message)), timeoutMs);
      Promise.resolve(promise).then(
        value => { clearTimeout(timer); resolve(value); },
        error => { clearTimeout(timer); reject(error); },
      );
    });

    try {
      const initialized = await withTimeout(
        Promise.resolve().then(() => initHandler(cloneInitial(initialState), { initAck })),
        'INIT_TIMEOUT_REFUSED',
        `initialization exceeded ${timeoutMs} ms`,
      );
      await withTimeout(
        ack,
        'INIT_ACK_TIMEOUT_REFUSED',
        `initAck was not observed within ${timeoutMs} ms`,
      );
      return Result.ok(Proc.spawn(initialized, loopHandler));
    } catch (error) {
      return Result.err(error);
    }
  },
});

export const CrashRecovery = Object.freeze({
  async retry(maxAttempts, supplier) {
    if (!Number.isInteger(maxAttempts) || maxAttempts < 1) {
      throw new RangeError('maxAttempts must be an integer of at least 1');
    }
    let lastError;
    for (let attempt = 1; attempt <= maxAttempts; attempt += 1) {
      try {
        const value = await Promise.resolve().then(() => supplier(attempt));
        return Result.ok(value);
      } catch (error) {
        lastError = error;
      }
    }
    return Result.err(lastError);
  },
});

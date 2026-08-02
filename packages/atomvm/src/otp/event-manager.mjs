import { Proc, deepFreeze, immutableMessage, OtpRefusal } from './process.mjs';

function normalizeHandler(handler) {
  if (typeof handler === 'function') {
    return { handleEvent: handler, terminate: () => {} };
  }
  if (!handler || typeof handler.handleEvent !== 'function') {
    throw new TypeError('handler must be a function or object with handleEvent(event)');
  }
  return {
    handleEvent: event => handler.handleEvent(event),
    terminate: reason => handler.terminate?.(reason),
    original: handler,
  };
}

export class EventManager {
  #handlers = [];
  #proc;

  constructor({ id = 'event-manager' } = {}) {
    this.#proc = Proc.spawn(
      deepFreeze({ delivered: 0, evicted: 0 }),
      async (state, command) => {
        if (command.type !== 'EVENT') return state;
        const survivors = [];
        let delivered = state.delivered;
        let evicted = state.evicted;
        for (const handler of this.#handlers) {
          try {
            await handler.handleEvent(command.event);
            survivors.push(handler);
            delivered += 1;
          } catch (error) {
            evicted += 1;
            try { await handler.terminate(error); } catch { /* termination cannot kill bus */ }
          }
        }
        this.#handlers = survivors;
        return deepFreeze({ delivered, evicted });
      },
      { id },
    );
  }

  static start(id) {
    return new EventManager({ id: id ?? 'event-manager' });
  }

  addHandler(handler) {
    const normalized = normalizeHandler(handler);
    if (this.#handlers.some(current => current.original === handler || current === handler)) {
      throw new OtpRefusal('DUPLICATE_HANDLER_REFUSED', 'event handler is already registered');
    }
    this.#handlers.push(normalized);
    return handler;
  }

  async deleteHandler(handler) {
    const index = this.#handlers.findIndex(current => current.original === handler || current === handler);
    if (index < 0) return false;
    const [removed] = this.#handlers.splice(index, 1);
    await removed.terminate(null);
    return true;
  }

  notify(event) {
    this.#proc.tell(immutableMessage('EVENT', { event }));
  }

  async syncNotify(event, timeoutMs = 5000) {
    await this.#proc.ask(immutableMessage('EVENT', { event }), timeoutMs);
  }

  handlerCount() {
    return this.#handlers.length;
  }

  async statistics() {
    return this.#proc.state();
  }

  async stop() {
    const handlers = this.#handlers.splice(0);
    for (const handler of handlers) {
      try { await handler.terminate(null); } catch { /* independent cleanup */ }
    }
    await this.#proc.stop();
  }

  proc() { return this.#proc; }
}

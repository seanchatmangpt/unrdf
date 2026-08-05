import { Proc, ProcSys, deepFreeze, immutableMessage, OtpRefusal } from './process.mjs';

export const Transition = Object.freeze({
  nextState(state, data) { return deepFreeze({ type: 'next_state', state, data }); },
  keepState(data) { return deepFreeze({ type: 'keep_state', data }); },
  stop(reason = 'normal', data = undefined) { return deepFreeze({ type: 'stop', reason, data }); },
});

export class StateMachine {
  #proc;
  #transition;

  constructor(initialState, initialData, transition, { id } = {}) {
    if (typeof transition !== 'function') throw new TypeError('transition must be a function');
    this.#transition = transition;
    this.#proc = Proc.spawn(
      deepFreeze({ state: initialState, data: initialData, running: true, stopReason: null }),
      async (machine, envelope) => {
        const result = await this.#transition(machine.state, envelope.event, machine.data);
        if (!result || typeof result !== 'object') {
          throw new OtpRefusal('INVALID_TRANSITION_REFUSED', 'state machine transition must return Transition.*');
        }
        switch (result.type) {
          case 'next_state':
            return deepFreeze({ state: result.state, data: result.data, running: true, stopReason: null });
          case 'keep_state':
            return deepFreeze({ state: machine.state, data: result.data, running: true, stopReason: null });
          case 'stop':
            queueMicrotask(() => void this.#proc.stop(result.reason));
            return deepFreeze({
              state: machine.state,
              data: result.data === undefined ? machine.data : result.data,
              running: false,
              stopReason: result.reason,
            });
          default:
            throw new OtpRefusal('INVALID_TRANSITION_REFUSED', `unknown transition ${result.type}`);
        }
      },
      { id },
    );
  }

  static create(initialState, initialData, transition, options) {
    return new StateMachine(initialState, initialData, transition, options);
  }

  send(event) {
    this.#proc.tell(immutableMessage('STATE_EVENT', { event }));
  }

  async call(event, timeoutMs = 5000) {
    const machine = await this.#proc.ask(immutableMessage('STATE_EVENT', { event }), timeoutMs);
    return machine.data;
  }

  async state() {
    return (await ProcSys.getState(this.#proc)).state;
  }

  async data() {
    return (await ProcSys.getState(this.#proc)).data;
  }

  isRunning() {
    return this.#proc.isRunning;
  }

  stop() {
    return this.#proc.stop();
  }

  proc() {
    return this.#proc;
  }
}

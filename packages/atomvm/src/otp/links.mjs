import { randomUUID } from 'node:crypto';
import { Proc } from './proc.mjs';
import { ProcRef } from './references.mjs';
import { DownSignal, normalizeExit, OtpRefusal } from './values.mjs';

export class MonitorRef {
  constructor(target, cancel, { id = `monitor-${randomUUID()}` } = {}) {
    this.id = id;
    this.target = target;
    this.cancel = cancel;
    Object.freeze(this);
  }
}

function resolveProc(target) {
  return target instanceof ProcRef ? target.proc() : target;
}

export const ProcMonitor = Object.freeze({
  monitor(target, downHandler) {
    const proc = resolveProc(target);
    if (!(proc instanceof Proc)) throw new TypeError('monitor target must be Proc or ProcRef');
    if (typeof downHandler !== 'function') throw new TypeError('downHandler must be a function');
    let active = true;
    let ref;
    const remove = proc.addTerminationCallback((reason, terminated, termination) => {
      if (!active) return;
      active = false;
      downHandler(reason, terminated, termination);
    });
    ref = new MonitorRef(proc, () => {
      if (!active) return false;
      active = false;
      remove();
      return true;
    });
    return ref;
  },

  monitorProcess(observerTarget, target) {
    const observer = resolveProc(observerTarget);
    const monitored = resolveProc(target);
    if (!(observer instanceof Proc) || !(monitored instanceof Proc)) {
      throw new TypeError('monitorProcess requires Proc or ProcRef observer and target');
    }
    let ref;
    ref = this.monitor(monitored, (_reason, terminated, termination) => {
      observer.tryTell(
        new DownSignal(ref.id, terminated.id, termination?.exit ?? normalizeExit(_reason, { target: terminated.id })),
        { from: terminated.id },
      );
    });
    return ref;
  },

  demonitor(ref) {
    return ref?.cancel?.() ?? false;
  },
});

const links = new WeakMap();
function linkSet(proc) {
  if (!links.has(proc)) links.set(proc, new Set());
  return links.get(proc);
}

export const ProcLink = Object.freeze({
  link(leftTarget, rightTarget) {
    const left = resolveProc(leftTarget);
    const right = resolveProc(rightTarget);
    if (!(left instanceof Proc) || !(right instanceof Proc)) throw new TypeError('link requires Proc or ProcRef');
    if (left === right) throw new OtpRefusal('SELF_LINK_REFUSED', 'a process cannot link to itself');
    if (linkSet(left).has(right)) return Object.freeze({ left, right, unlink: () => false });

    let active = true;
    linkSet(left).add(right);
    linkSet(right).add(left);
    const removeLeft = left.addTerminationCallback((reason, _proc, termination) => {
      if (active && right.isRunning) right.deliverExitSignal(termination?.exit ?? reason ?? 'normal', left.id);
    });
    const removeRight = right.addTerminationCallback((reason, _proc, termination) => {
      if (active && left.isRunning) left.deliverExitSignal(termination?.exit ?? reason ?? 'normal', right.id);
    });
    const unlink = () => {
      if (!active) return false;
      active = false;
      linkSet(left).delete(right);
      linkSet(right).delete(left);
      removeLeft();
      removeRight();
      return true;
    };
    return Object.freeze({ left, right, unlink });
  },

  spawnLink(parent, initialState, handler, options) {
    const child = Proc.spawn(initialState, handler, options);
    this.link(parent, child);
    return child;
  },
});

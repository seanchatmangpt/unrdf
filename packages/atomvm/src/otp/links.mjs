import { randomUUID } from 'node:crypto';
import { Proc } from './proc.mjs';
import { ProcRef } from './references.mjs';
import { OtpRefusal } from './values.mjs';

export class MonitorRef {
  constructor(target, cancel) {
    this.id = `monitor-${randomUUID()}`;
    this.target = target;
    this.cancel = cancel;
  }
}

export const ProcMonitor = Object.freeze({
  monitor(target, downHandler) {
    const proc = target instanceof ProcRef ? target.proc() : target;
    if (!(proc instanceof Proc)) throw new TypeError('monitor target must be Proc or ProcRef');
    if (typeof downHandler !== 'function') throw new TypeError('downHandler must be a function');
    let active = true;
    const remove = proc.addTerminationCallback(reason => {
      if (!active) return;
      active = false;
      downHandler(reason, proc);
    });
    return new MonitorRef(proc, () => {
      if (!active) return false;
      active = false;
      remove();
      return true;
    });
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
    const left = leftTarget instanceof ProcRef ? leftTarget.proc() : leftTarget;
    const right = rightTarget instanceof ProcRef ? rightTarget.proc() : rightTarget;
    if (!(left instanceof Proc) || !(right instanceof Proc)) throw new TypeError('link requires Proc or ProcRef');
    if (left === right) throw new OtpRefusal('SELF_LINK_REFUSED', 'a process cannot link to itself');
    if (linkSet(left).has(right)) return Object.freeze({ left, right, unlink: () => false });

    let active = true;
    linkSet(left).add(right);
    linkSet(right).add(left);
    const removeLeft = left.addTerminationCallback(reason => {
      if (active && reason && right.isRunning) right.deliverExitSignal(reason, left.id);
    });
    const removeRight = right.addTerminationCallback(reason => {
      if (active && reason && left.isRunning) left.deliverExitSignal(reason, right.id);
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

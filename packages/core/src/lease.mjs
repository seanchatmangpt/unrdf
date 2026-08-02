/** In-memory lease registry with fencing tokens. */
export class LeaseRegistry {
  #leases = new Map();
  #token = 0;

  constructor({ now = () => Date.now() } = {}) {
    this.now = now;
  }

  acquire(resource, owner, ttlMs) {
    if (!resource || !owner || !Number.isFinite(ttlMs) || ttlMs <= 0) throw new TypeError('resource, owner, and positive ttlMs are required');
    const current = this.#leases.get(resource);
    if (current && current.expiresAt > this.now() && current.owner !== owner) return null;
    const lease = { resource, owner, token: ++this.#token, expiresAt: this.now() + ttlMs };
    this.#leases.set(resource, lease);
    return { ...lease };
  }

  renew(resource, owner, token, ttlMs) {
    const lease = this.#leases.get(resource);
    if (!lease || lease.owner !== owner || lease.token !== token || lease.expiresAt <= this.now()) return null;
    lease.expiresAt = this.now() + ttlMs;
    return { ...lease };
  }

  release(resource, owner, token) {
    const lease = this.#leases.get(resource);
    if (!lease || lease.owner !== owner || lease.token !== token) return false;
    return this.#leases.delete(resource);
  }

  inspect(resource) {
    const lease = this.#leases.get(resource);
    if (!lease) return null;
    if (lease.expiresAt <= this.now()) {
      this.#leases.delete(resource);
      return null;
    }
    return { ...lease };
  }
}

export function createLeaseRegistry(options) { return new LeaseRegistry(options); }

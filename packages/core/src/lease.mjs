/** In-memory lease registry with fencing tokens. */
export class LeaseRegistry {
  #leases = new Map();
  #token = 0;

  /**
   * Create a registry.
   * @param {Object} [options] - Options.
   * @param {Function} [options.now] - Clock returning the current time in milliseconds.
   */
  constructor({ now = () => Date.now() } = {}) {
    this.now = now;
  }

  /**
   * Acquire or re-acquire a lease, issuing a new fencing token.
   * @param {string} resource - Resource name.
   * @param {string} owner - Requesting owner.
   * @param {number} ttlMs - Positive lease duration in milliseconds.
   * @returns {Object|null} Copy of the lease {resource, owner, token, expiresAt}, or null if another owner holds a live lease.
   * @throws {TypeError} If arguments are invalid.
   */
  acquire(resource, owner, ttlMs) {
    if (!resource || !owner || !Number.isFinite(ttlMs) || ttlMs <= 0) throw new TypeError('resource, owner, and positive ttlMs are required');
    const current = this.#leases.get(resource);
    if (current && current.expiresAt > this.now() && current.owner !== owner) return null;
    const lease = { resource, owner, token: ++this.#token, expiresAt: this.now() + ttlMs };
    this.#leases.set(resource, lease);
    return { ...lease };
  }

  /**
   * Extend a live lease held by the owner with the matching token.
   * @param {string} resource - Resource name.
   * @param {string} owner - Lease owner.
   * @param {number} token - Fencing token from acquire.
   * @param {number} ttlMs - New duration in milliseconds from now.
   * @returns {Object|null} Copy of the renewed lease, or null if it is missing, expired or not matched.
   */
  renew(resource, owner, token, ttlMs) {
    const lease = this.#leases.get(resource);
    if (!lease || lease.owner !== owner || lease.token !== token || lease.expiresAt <= this.now()) return null;
    lease.expiresAt = this.now() + ttlMs;
    return { ...lease };
  }

  /**
   * Release a lease held by the owner with the matching token.
   * @param {string} resource - Resource name.
   * @param {string} owner - Lease owner.
   * @param {number} token - Fencing token from acquire.
   * @returns {boolean} True if the lease was released.
   */
  release(resource, owner, token) {
    const lease = this.#leases.get(resource);
    if (!lease || lease.owner !== owner || lease.token !== token) return false;
    return this.#leases.delete(resource);
  }

  /**
   * Look up the current lease, dropping it if expired.
   * @param {string} resource - Resource name.
   * @returns {Object|null} Copy of the live lease, or null.
   */
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

/**
 * Create a LeaseRegistry.
 * @param {Object} [options] - Options forwarded to the constructor.
 * @returns {LeaseRegistry} A new registry.
 */
export function createLeaseRegistry(options) { return new LeaseRegistry(options); }

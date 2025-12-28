import { z } from 'zod';

const PermissionPolicySchema = z.object({
  universe_id: z.string().min(1),
  admissible_actors: z.array(z.string()),
  operations: z.array(z.enum(['appendEvent', 'freeze', 'seal', 'read'])),
  created_at: z.string(),
});

export class PermissionGuard {
  constructor() {
    this.policies = new Map();
  }

  registerPolicy(policy) {
    const validated = PermissionPolicySchema.parse(policy);
    this.policies.set(validated.universe_id, validated);
  }

  guard(actor_id, operation, universe_id) {
    const policy = this.policies.get(universe_id);
    if (!policy) {
      throw new Error('Permission denied: No policy found for universe ' + universe_id);
    }
    if (!policy.admissible_actors.includes(actor_id)) {
      throw new Error('Permission denied: Actor ' + actor_id + ' not in admissible_actors');
    }
    if (!policy.operations.includes(operation)) {
      throw new Error('Permission denied: Operation ' + operation + ' not allowed');
    }
    return true;
  }

  check(actor_id, operation, universe_id) {
    try {
      this.guard(actor_id, operation, universe_id);
      return { allowed: true };
    } catch (error) {
      return { allowed: false, reason: error.message };
    }
  }

  getPolicy(universe_id) {
    return this.policies.get(universe_id) || null;
  }

  removePolicy(universe_id) {
    this.policies.delete(universe_id);
  }
}

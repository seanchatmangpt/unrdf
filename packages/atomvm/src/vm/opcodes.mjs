/**
 * POWL8 Hardware Abstraction Layer (HAL)
 * Native concurrency opcodes mapped directly into the VM.
 */

export const POWL8_OPCODES = {
  OP_SPAWN_SEQ: 0xA0,
  OP_SPAWN_CHOICE: 0xA1,
  OP_LOOP_START: 0xA2,
  OP_LOOP_END: 0xA3,
  OP_AWAIT_SYNC: 0xA4,
};

export function registerPowl8Opcodes(vm) {
  Object.entries(POWL8_OPCODES).forEach(([name, code]) => {
    vm.registerOpcode(name, code);
  });
}

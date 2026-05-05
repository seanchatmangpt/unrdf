/**
 * L4 Hardened AtomVM Facade
 * 
 * Provides a high-level Developer Experience (DX) and Quality of Life (QoL)
 * API to interact with the hardened AtomVM, establishing a robust,
 * bidirectional JS -> Erlang -> JS execution loop.
 */
import { ConstitutionalLoader } from './loader.mjs';
import { SecuritySandbox } from './sandbox.mjs';
import { VmTelemetry } from './telemetry.mjs';
import { ReceiptGenerator } from './receipt-generator.mjs';
import { HooksBridge } from '../hooks-bridge.mjs';
import { AtomVMNodeRuntime } from '../node-runtime.mjs';
import { JITRegulatoryCompiler } from '../compiler/jit-regulatory.mjs';
import { registerPowl8Opcodes } from './opcodes.mjs';

export class HardenedAtomVM {
  /**
   * Initializes the Hardened AtomVM.
   * 
   * @param {Object} store - The central KGCStore instance.
   * @param {Object} policyGraph - The ODRL policy graph for the sandbox.
   * @param {Object} options - Configuration options.
   */
  constructor(store, policyGraph = {}, options = {}) {
    if (!store) throw new Error('A valid KGCStore must be provided for the JS->Erlang bridge.');
    
    this.store = store;
    this.telemetry = new VmTelemetry();
    this.sandbox = new SecuritySandbox(policyGraph);
    this.receiptGenerator = new ReceiptGenerator();
    this.jitCompiler = new JITRegulatoryCompiler(options);
    
    // Register custom opcodes if store has registration capability
    if (typeof store.registerOpcode === 'function') {
      registerPowl8Opcodes(store);
    }
    
    // Create the real JS->Erlang bidirectional bridge
    this.bridge = new HooksBridge(this.store, {
      nodeId: options.nodeId || 'hardened-vm-node',
      enableReceiptChaining: true,
      enableJIT: true
    });

    const defaultLog = options.log || console.log;

    // Initialize the Node runtime for Erlang bytecode
    this.runtime = options.runtime || new AtomVMNodeRuntime({
      log: (msg) => {
        // Intercept bridge messages directly from the VM's stdout wrapper
        if (typeof msg === 'string' && msg.includes('KGC4D_BRIDGE:')) {
           this._handleErlangMessage(msg);
        }
        defaultLog(msg);
      },
      errorLog: options.errorLog || console.error
    });

    this.loader = new ConstitutionalLoader(this.runtime, options.loaderOptions);
  }

  /**
   * Prepares the runtime and loads the Erlang VM base.
   */
  async initialize() {
    await this.runtime.load();
  }

  /**
   * Safe execution entry point. Loads bytecode constitutionally and executes it
   * within the Erlang VM, wiring up the JS/Erlang bridge for side-effects.
   * 
   * @param {string} avmPath - Path to the compiled BEAM bytecode.
   * @param {Object} specKitReceipt - Cryptographic receipt verifying the compilation.
   * @param {Object} agentId - The identity of the executing agent.
   * @param {Object} inputContext - The contextual input to the VM.
   * @returns {Promise<Object>} Execution result and the PROV-O receipt.
   */
  async execute(avmPath, specKitReceipt, agentId, inputContext = {}) {
    const startTime = performance.now();
    
    try {
      // 1. Constitutional Load: Validates the cryptographic receipt
      await this.loader.loadBytecode(avmPath, specKitReceipt);

      // 2. Sandbox Inspection: Intercept opcodes if supported by the runtime
      if (typeof this.runtime.intercept === 'function') {
        this.runtime.intercept((opcodes) => this.sandbox.interceptOpcodes(opcodes));
      }

      // 3. Execution: Pass POWL8 DAG payload via env if provided
      if (inputContext.isPowl8Dag && inputContext.taskNodes) {
        process.env.POWL8_DAG_PAYLOAD = JSON.stringify(inputContext.taskNodes);
      }

      // Execute actual AtomVM process
      const resultState = await this.runtime.execute(avmPath);

      // 4. Receipt Generation: Create deterministic execution receipt
      const receipt = await this.receiptGenerator.generateExecutionReceipt(
        inputContext,
        agentId,
        resultState
      );

      // 5. Telemetry
      this.telemetry.recordExecutionTime(startTime);
      this.telemetry.recordOpcodesProcessed(1); // Abstract representation of VM completion

      return {
        success: resultState.exitCode === 0,
        state: resultState,
        receipt
      };

    } catch (error) {
      this.telemetry.recordExecutionTime(startTime);
      throw error;
    }
  }

  /**
   * JIT Compiles and enforces a regulatory SHACL shape.
   * 
   * @param {string} moduleName - Name of the regulatory module.
   * @param {Object} shape - The SHACL shape definition.
   * @returns {Promise<Object>} The compilation and registration result.
   */
  async enforceRegulatoryShape(moduleName, shape) {
    const erlangSource = this.jitCompiler.compileToErlang(moduleName, shape);
    const bytecode = await this.jitCompiler.compileToBytecode(erlangSource);
    
    // Get the SPARQL query that was used for the condition
    const sparql = this.jitCompiler.shaclCompiler.compileToASK(shape);

    // In a real system, we'd write this to a .beam file and hot-load it.
    // Here we'll register it as a "hook" or "resident validator" via the bridge.
    await this.bridge.registerHook({
      hook_name: moduleName,
      hook_type: 'validation',
      condition: {
        type: 'sparql-ask',
        spec: sparql
      },
      effects: [],
      meta: {
        source: erlangSource,
        bytecode: Buffer.from(bytecode).toString('base64')
      }
    });

    return {
      success: true,
      moduleName,
      source: erlangSource,
      sparql
    };
  }

  /**
   * @private
   */
  _handleErlangMessage(text) {
    // Extract the KGC4D_BRIDGE command part
    const bridgeCommand = text.substring(text.indexOf('KGC4D_BRIDGE:'));
    const parts = bridgeCommand.split(':');
    if (parts.length < 3) return;

    const command = parts[1];
    
    if (command === 'register_hook') {
      try {
        const hookSpecStr = parts.slice(2).join(':');
        // A minimal parse, assumes JSON string representation passed in Erlang logs
        // e.g. KGC4D_BRIDGE:register_hook:{"hook_name":"example","hook_type":"validation",...}
        const hookSpec = JSON.parse(hookSpecStr);
        this.bridge.registerHook(hookSpec).catch(console.error);
      } catch (e) {
        console.error('Failed to parse register_hook payload:', e);
      }
    } else if (command === 'execute_effect') {
      try {
        const effectSpecStr = parts.slice(2).join(':');
        const effectSpec = JSON.parse(effectSpecStr);
        this.bridge.executeEffect(effectSpec).catch(console.error);
      } catch (e) {
        console.error('Failed to parse execute_effect payload:', e);
      }
    }
  }
}

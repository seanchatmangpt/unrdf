/**
 * Workflow Verifier - JavaScript wrapper for WorkflowVerifier smart contract
 *
 * Provides high-level API for interacting with the deployed WorkflowVerifier contract.
 * Handles contract deployment, interaction, and event monitoring.
 *
 * @module @unrdf/blockchain/contracts
 */

import { ethers } from 'ethers';
import { z } from 'zod';

// =============================================================================
// Schemas
// =============================================================================

const ContractStatsSchema = z.object({
  totalAnchored: z.number(),
  totalMerkleRoots: z.number(),
  owner: z.string(),
});

// =============================================================================
// WorkflowVerifier Contract Wrapper
// =============================================================================

/**
 * WorkflowVerifier - High-level wrapper for smart contract interaction
 *
 * @class
 * @example
 * ```javascript
 * const verifier = new WorkflowVerifier({
 *   provider: 'http://localhost:8545',
 *   privateKey: process.env.PRIVATE_KEY
 * });
 *
 * // Deploy contract
 * const address = await verifier.deploy();
 *
 * // Or connect to existing
 * verifier.connect(existingAddress);
 *
 * // Use contract
 * const stats = await verifier.getStats();
 * ```
 */
export class WorkflowVerifier {
  /**
   * @param {Object} config - Configuration
   * @param {string} config.provider - Ethereum provider URL
   * @param {string} config.privateKey - Private key for signing
   * @param {string} [config.contractAddress] - Existing contract address
   */
  constructor({ provider, privateKey, contractAddress }) {
    this.provider = new ethers.JsonRpcProvider(provider);
    this.wallet = new ethers.Wallet(privateKey, this.provider);
    this.contractAddress = contractAddress;
    this.contract = null;

    if (contractAddress) {
      this.connect(contractAddress);
    }
  }

  /**
   * Deploy new WorkflowVerifier contract
   *
   * @returns {Promise<string>} Deployed contract address
   */
  async deploy() {
    // Contract bytecode and ABI
    const abi = this._getABI();
    const bytecode = this._getBytecode();

    // Deploy contract
    const factory = new ethers.ContractFactory(abi, bytecode, this.wallet);
    const contract = await factory.deploy();
    await contract.waitForDeployment();

    this.contractAddress = await contract.getAddress();
    this.contract = contract;

    return this.contractAddress;
  }

  /**
   * Connect to existing contract
   *
   * @param {string} address - Contract address
   */
  connect(address) {
    this.contractAddress = address;
    const abi = this._getABI();
    this.contract = new ethers.Contract(address, abi, this.wallet);
  }

  /**
   * Get contract statistics
   *
   * @returns {Promise<Object>} Contract stats
   */
  async getStats() {
    if (!this.contract) {
      throw new Error('Contract not connected. Call deploy() or connect() first.');
    }

    const [totalAnchored, totalMerkleRoots, owner] = await this.contract.getStats();

    const stats = {
      totalAnchored: Number(totalAnchored),
      totalMerkleRoots: Number(totalMerkleRoots),
      owner,
    };

    return ContractStatsSchema.parse(stats);
  }

  /**
   * Listen for ReceiptAnchored events
   *
   * @param {Function} callback - Called with (receiptHash, blockNumber, timestamp)
   */
  onReceiptAnchored(callback) {
    if (!this.contract) {
      throw new Error('Contract not connected');
    }

    this.contract.on('ReceiptAnchored', (receiptHash, blockNumber, timestamp) => {
      callback({
        receiptHash,
        blockNumber: Number(blockNumber),
        timestamp: Number(timestamp),
      });
    });
  }

  /**
   * Listen for BatchAnchored events
   *
   * @param {Function} callback - Called with event data
   */
  onBatchAnchored(callback) {
    if (!this.contract) {
      throw new Error('Contract not connected');
    }

    this.contract.on('BatchAnchored', (receiptHashes, blockNumber, timestamp) => {
      callback({
        receiptHashes,
        blockNumber: Number(blockNumber),
        timestamp: Number(timestamp),
      });
    });
  }

  /**
   * Listen for MerkleRootAnchored events
   *
   * @param {Function} callback - Called with event data
   */
  onMerkleRootAnchored(callback) {
    if (!this.contract) {
      throw new Error('Contract not connected');
    }

    this.contract.on('MerkleRootAnchored', (merkleRoot, receiptCount, blockNumber, timestamp) => {
      callback({
        merkleRoot,
        receiptCount: Number(receiptCount),
        blockNumber: Number(blockNumber),
        timestamp: Number(timestamp),
      });
    });
  }

  /**
   * Transfer contract ownership
   *
   * @param {string} newOwner - New owner address
   * @returns {Promise<Object>} Transaction receipt
   */
  async transferOwnership(newOwner) {
    if (!this.contract) {
      throw new Error('Contract not connected');
    }

    const tx = await this.contract.transferOwnership(newOwner);
    return await tx.wait();
  }

  /**
   * Get contract ABI
   *
   * @private
   * @returns {Array} Contract ABI
   */
  _getABI() {
    return [
      'constructor()',
      'function owner() view returns (address)',
      'function totalAnchored() view returns (uint256)',
      'function totalMerkleRoots() view returns (uint256)',
      'function anchorReceipt(bytes32 receiptHash) returns (bool)',
      'function anchorBatch(bytes32[] memory receiptHashes) returns (bool)',
      'function anchorMerkleRoot(bytes32 merkleRoot, uint256 receiptCount) returns (bool)',
      'function verifyReceipt(bytes32 receiptHash) view returns (bool, uint256, bytes32)',
      'function verifyMerkleRoot(bytes32 merkleRoot) view returns (bool, uint256, uint256)',
      'function getAnchorDetails(bytes32 receiptHash) view returns (tuple(uint256 blockNumber, uint256 timestamp, bytes32 txHash, bool exists))',
      'function getStats() view returns (uint256, uint256, address)',
      'function transferOwnership(address newOwner)',
      'event ReceiptAnchored(bytes32 indexed receiptHash, uint256 blockNumber, uint256 timestamp)',
      'event BatchAnchored(bytes32[] receiptHashes, uint256 blockNumber, uint256 timestamp)',
      'event MerkleRootAnchored(bytes32 indexed merkleRoot, uint256 receiptCount, uint256 blockNumber, uint256 timestamp)',
      'event OwnershipTransferred(address indexed previousOwner, address indexed newOwner)',
    ];
  }

  /**
   * Get contract bytecode
   *
   * NOTE: This is a simplified bytecode. In production, use the compiled output
   * from Hardhat/Foundry. For demo purposes, we'll use a minimal version.
   *
   * @private
   * @returns {string} Contract bytecode
   */
  _getBytecode() {
    // This is a placeholder. In real deployment, you'd use the compiled bytecode
    // from `solc` or Hardhat. For the demo, we'll provide instructions to compile.
    return '0x608060405234801561001057600080fd5b50600080546001600160a01b03191633179055600060018190556002556107d1806100876000396000f3fe608060405234801561001057600080fd5b50600436106100a95760003560e01c8063715018a611610071578063715018a6146101635780638da5cb5b14610175578063b8a7d4de1461019d578063c4e41b22146101b0578063d547cfb7146101b8578063f2fde38b146101cb57600080fd5b80630c55699c146100ae57806319ab453c146100ca5780632b23c99b146100dd5780634ee2cd7e1461011d5780636352211e14610150575b600080fd5b6100b7600154565b6040519081526020015b60405180910390f35b6100b76100d8366004610662565b6101de565b6101006100eb366004610684565b60036020526000908152604090205460ff1681565b60405190151581526020016100c1565b61013061012b3660046106a6565b610253565b604080519315158452602084019290925290820152606001610c1565b61010061015e366004610662565b6102c9565b61016d610334565b005b600054610188906001600160a01b031681565b6040516001600160a01b0390911681526020016100c1565b61016d6101ab366004610662565b6103a8565b6100b7610497565b6101886101c6366004610662565b6104a7565b61016d6101d93660046106c8565b6104d1565b60008181526003602052604081205460ff166102415760405162461bcd60e51b815260206004820152601760248201527f526563656970742068617368206e6f7420666f756e6400000000000000000060448201526064015b60405180910390fd5b5060009081526003602052604090205490565b600080600061026185610599565b90508061028557506000915081905080610c1565b5050600093845260036020908152604080862054600484528287205460059094529190942054919390925050565b600081815260036020526040812054819060ff166103295760405162461bcd60e51b815260206004820152601f60248201527f526563656970742068617368206e6f7420666f756e6420696e20636861696e0060448201526064016102385795909450925050565b5060019392505050565b6000546001600160a01b031633146103885760405162461bcd60e51b815260206004820181905260248201527f4f776e61626c653a2063616c6c6572206973206e6f7420746865206f776e65726044820152606401610238565b600080546040516001600160a01b0390911690600080516020610778833981519152908390a3565b6000546001600160a01b031633146104025760405162461bcd60e51b815260206004820181905260248201527f4f776e61626c653a2063616c6c6572206973206e6f7420746865206f776e65726044820152606401610238565b60008181526003602052604090205460ff16156104615760405162461bcd60e51b815260206004820152601960248201527f526563656970742068617368206578697374732061667465720000000000006044820152606401610238565b60008181526003602052604090819020805460ff19166001179055600180549161048a836106f3565b9190505550600255565b60006104a260015490565b905090565b60006104b282610599565b6104be575060006104cc565b5050600090815260056020526040902054905b919050565b6000546001600160a01b0316331461052b5760405162461bcd60e51b815260206004820181905260248201527f4f776e61626c653a2063616c6c6572206973206e6f7420746865206f776e65726044820152606401610238565b6001600160a01b0381166105815760405162461bcd60e51b815260206004820152601f60248201527f4f776e61626c653a206e6577206f776e657220697320746865207a65726f00604482015260640161023856';
  }
}

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Estimate gas costs for different operations
 *
 * @param {Object} provider - Ethereum provider
 * @returns {Promise<Object>} Gas cost estimates
 */
export async function estimateGasCosts(provider) {
  const ethProvider = new ethers.JsonRpcProvider(provider);
  const feeData = await ethProvider.getFeeData();

  const estimates = {
    deployment: {
      gas: 500000n,
      costWei: 500000n * feeData.gasPrice,
      costETH: ethers.formatEther(500000n * feeData.gasPrice),
    },
    singleAnchor: {
      gas: 50000n,
      costWei: 50000n * feeData.gasPrice,
      costETH: ethers.formatEther(50000n * feeData.gasPrice),
    },
    batchAnchor: (count) => ({
      gas: 30000n + BigInt(count) * 20000n,
      costWei: (30000n + BigInt(count) * 20000n) * feeData.gasPrice,
      costETH: ethers.formatEther((30000n + BigInt(count) * 20000n) * feeData.gasPrice),
    }),
    merkleAnchor: {
      gas: 60000n,
      costWei: 60000n * feeData.gasPrice,
      costETH: ethers.formatEther(60000n * feeData.gasPrice),
    },
  };

  return estimates;
}

// =============================================================================
// Exports
// =============================================================================

export default WorkflowVerifier;
export { ContractStatsSchema };

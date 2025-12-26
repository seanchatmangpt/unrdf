// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

/**
 * WorkflowVerifier - Smart contract for YAWL receipt verification
 *
 * Provides on-chain anchoring and verification of workflow receipt hashes.
 * Supports individual receipts, batch anchoring, and Merkle root anchoring.
 *
 * @title WorkflowVerifier
 * @author UNRDF Team
 */
contract WorkflowVerifier {
    // =============================================================================
    // State Variables
    // =============================================================================

    /// Owner of the contract
    address public owner;

    /// Mapping: receiptHash => AnchorRecord
    mapping(bytes32 => AnchorRecord) private anchoredReceipts;

    /// Mapping: merkleRoot => MerkleAnchor
    mapping(bytes32 => MerkleAnchor) private anchoredMerkleRoots;

    /// Total number of anchored receipts
    uint256 public totalAnchored;

    /// Total number of Merkle roots anchored
    uint256 public totalMerkleRoots;

    // =============================================================================
    // Structs
    // =============================================================================

    /**
     * Record of an anchored receipt
     */
    struct AnchorRecord {
        uint256 blockNumber;
        uint256 timestamp;
        bytes32 txHash;
        bool exists;
    }

    /**
     * Record of an anchored Merkle root
     */
    struct MerkleAnchor {
        uint256 blockNumber;
        uint256 timestamp;
        uint256 receiptCount;
        bool exists;
    }

    // =============================================================================
    // Events
    // =============================================================================

    event ReceiptAnchored(
        bytes32 indexed receiptHash,
        uint256 blockNumber,
        uint256 timestamp
    );

    event BatchAnchored(
        bytes32[] receiptHashes,
        uint256 blockNumber,
        uint256 timestamp
    );

    event MerkleRootAnchored(
        bytes32 indexed merkleRoot,
        uint256 receiptCount,
        uint256 blockNumber,
        uint256 timestamp
    );

    event OwnershipTransferred(
        address indexed previousOwner,
        address indexed newOwner
    );

    // =============================================================================
    // Modifiers
    // =============================================================================

    modifier onlyOwner() {
        require(msg.sender == owner, "Only owner can call this function");
        _;
    }

    // =============================================================================
    // Constructor
    // =============================================================================

    constructor() {
        owner = msg.sender;
        totalAnchored = 0;
        totalMerkleRoots = 0;
    }

    // =============================================================================
    // Anchoring Functions
    // =============================================================================

    /**
     * Anchor a single receipt hash
     *
     * @param receiptHash - BLAKE3 hash of the receipt
     * @return success - Whether anchoring succeeded
     */
    function anchorReceipt(bytes32 receiptHash) external returns (bool) {
        require(receiptHash != bytes32(0), "Invalid receipt hash");
        require(!anchoredReceipts[receiptHash].exists, "Receipt already anchored");

        // Store anchor record
        anchoredReceipts[receiptHash] = AnchorRecord({
            blockNumber: block.number,
            timestamp: block.timestamp,
            txHash: bytes32(uint256(uint160(msg.sender))), // Simplified tx reference
            exists: true
        });

        totalAnchored++;

        emit ReceiptAnchored(receiptHash, block.number, block.timestamp);

        return true;
    }

    /**
     * Anchor multiple receipt hashes in a single transaction
     *
     * @param receiptHashes - Array of receipt hashes
     * @return success - Whether batch anchoring succeeded
     */
    function anchorBatch(bytes32[] memory receiptHashes) external returns (bool) {
        require(receiptHashes.length > 0, "Empty batch");
        require(receiptHashes.length <= 100, "Batch too large (max 100)");

        for (uint256 i = 0; i < receiptHashes.length; i++) {
            bytes32 receiptHash = receiptHashes[i];
            require(receiptHash != bytes32(0), "Invalid receipt hash in batch");

            // Skip if already anchored
            if (anchoredReceipts[receiptHash].exists) {
                continue;
            }

            anchoredReceipts[receiptHash] = AnchorRecord({
                blockNumber: block.number,
                timestamp: block.timestamp,
                txHash: bytes32(uint256(uint160(msg.sender))),
                exists: true
            });

            totalAnchored++;
        }

        emit BatchAnchored(receiptHashes, block.number, block.timestamp);

        return true;
    }

    /**
     * Anchor a Merkle tree root (most gas-efficient for large batches)
     *
     * @param merkleRoot - Root hash of the Merkle tree
     * @param receiptCount - Number of receipts in the tree
     * @return success - Whether anchoring succeeded
     */
    function anchorMerkleRoot(bytes32 merkleRoot, uint256 receiptCount) external returns (bool) {
        require(merkleRoot != bytes32(0), "Invalid Merkle root");
        require(receiptCount > 0, "Receipt count must be positive");
        require(!anchoredMerkleRoots[merkleRoot].exists, "Merkle root already anchored");

        anchoredMerkleRoots[merkleRoot] = MerkleAnchor({
            blockNumber: block.number,
            timestamp: block.timestamp,
            receiptCount: receiptCount,
            exists: true
        });

        totalMerkleRoots++;

        emit MerkleRootAnchored(merkleRoot, receiptCount, block.number, block.timestamp);

        return true;
    }

    // =============================================================================
    // Verification Functions
    // =============================================================================

    /**
     * Verify if a receipt hash is anchored on-chain
     *
     * @param receiptHash - Receipt hash to verify
     * @return exists - Whether the receipt is anchored
     * @return blockNumber - Block number where anchored (0 if not found)
     * @return txHash - Transaction reference (bytes32(0) if not found)
     */
    function verifyReceipt(bytes32 receiptHash)
        external
        view
        returns (bool exists, uint256 blockNumber, bytes32 txHash)
    {
        AnchorRecord memory record = anchoredReceipts[receiptHash];
        return (record.exists, record.blockNumber, record.txHash);
    }

    /**
     * Verify if a Merkle root is anchored
     *
     * @param merkleRoot - Merkle root to verify
     * @return exists - Whether the root is anchored
     * @return blockNumber - Block number where anchored
     * @return receiptCount - Number of receipts in the tree
     */
    function verifyMerkleRoot(bytes32 merkleRoot)
        external
        view
        returns (bool exists, uint256 blockNumber, uint256 receiptCount)
    {
        MerkleAnchor memory anchor = anchoredMerkleRoots[merkleRoot];
        return (anchor.exists, anchor.blockNumber, anchor.receiptCount);
    }

    /**
     * Get full anchor details for a receipt
     *
     * @param receiptHash - Receipt hash to query
     * @return record - Full anchor record
     */
    function getAnchorDetails(bytes32 receiptHash)
        external
        view
        returns (AnchorRecord memory record)
    {
        return anchoredReceipts[receiptHash];
    }

    // =============================================================================
    // Admin Functions
    // =============================================================================

    /**
     * Transfer ownership of the contract
     *
     * @param newOwner - Address of new owner
     */
    function transferOwnership(address newOwner) external onlyOwner {
        require(newOwner != address(0), "Invalid new owner");
        address previousOwner = owner;
        owner = newOwner;
        emit OwnershipTransferred(previousOwner, newOwner);
    }

    /**
     * Get contract statistics
     *
     * @return _totalAnchored - Total receipts anchored
     * @return _totalMerkleRoots - Total Merkle roots anchored
     * @return _owner - Contract owner address
     */
    function getStats()
        external
        view
        returns (uint256 _totalAnchored, uint256 _totalMerkleRoots, address _owner)
    {
        return (totalAnchored, totalMerkleRoots, owner);
    }
}

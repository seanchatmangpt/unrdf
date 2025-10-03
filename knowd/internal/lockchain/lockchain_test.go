package lockchain

import (
	"strings"
	"testing"
	"time"
)

func TestNew(t *testing.T) {
	config := Config{
		SigningKeyFile: "",
		PublicKeyFile:  "",
		ReceiptsDir:    "./test-receipts",
	}

	lc, err := New(config)
	if err != nil {
		t.Fatalf("Failed to create lockchain: %v", err)
	}

	if lc == nil {
		t.Fatal("New() returned nil")
	}
}

func TestWriteReceipt(t *testing.T) {
	config := Config{
		ReceiptsDir: "./test-receipts",
	}

	lc, err := New(config)
	if err != nil {
		t.Fatalf("Failed to create lockchain: %v", err)
	}

	delta := map[string]interface{}{
		"add": []map[string]interface{}{
			{
				"subject":   "http://example.org/person1",
				"predicate": "http://xmlns.com/foaf/0.1/name",
				"object":    "John Doe",
				"graph":     "default",
			},
		},
		"rem": []interface{}{},
	}

	receipt, err := lc.WriteReceipt("testActor", delta)
	if err != nil {
		t.Fatalf("Failed to write receipt: %v", err)
	}

	if receipt == nil {
		t.Fatal("WriteReceipt returned nil")
	}

	if receipt.ID == "" {
		t.Error("Receipt ID should not be empty")
	}

	if receipt.Actor != "testActor" {
		t.Errorf("Expected actor 'testActor', got '%s'", receipt.Actor)
	}

	if receipt.Version != "1" {
		t.Errorf("Expected version '1', got '%s'", receipt.Version)
	}

	if receipt.MerkleRoot == "" {
		t.Error("MerkleRoot should not be empty")
	}

	if receipt.Signature == "" {
		t.Error("Signature should not be empty")
	}

	if receipt.PubKey == "" {
		t.Error("PubKey should not be empty")
	}

	// Verify timestamp is recent
	timestamp, err := time.Parse(time.RFC3339, receipt.Timestamp)
	if err != nil {
		t.Fatalf("Failed to parse timestamp: %v", err)
	}

	if time.Since(timestamp) > time.Minute {
		t.Error("Timestamp should be recent")
	}
}

func TestVerifyReceipt(t *testing.T) {
	config := Config{
		ReceiptsDir: "./test-receipts",
	}

	lc, err := New(config)
	if err != nil {
		t.Fatalf("Failed to create lockchain: %v", err)
	}

	delta := map[string]interface{}{
		"add": []map[string]interface{}{
			{
				"subject":   "http://example.org/person1",
				"predicate": "http://xmlns.com/foaf/0.1/name",
				"object":    "John Doe",
				"graph":     "default",
			},
		},
	}

	receipt, err := lc.WriteReceipt("testActor", delta)
	if err != nil {
		t.Fatalf("Failed to write receipt: %v", err)
	}

	// Verify the receipt
	err = lc.Verify(receipt)
	if err != nil {
		t.Fatalf("Receipt verification failed: %v", err)
	}

	// Test with modified receipt (should fail)
	tamperedReceipt := *receipt
	tamperedReceipt.MerkleRoot = "fake_hash"

	err = lc.Verify(&tamperedReceipt)
	if err == nil {
		t.Error("Expected verification to fail for tampered receipt")
	}
}

func TestCanonicalization(t *testing.T) {
	urdna := NewURDNA2015()

	// Test data with blank nodes
	testQuads := [][]string{
		{"_:b1", "http://xmlns.com/foaf/0.1/name", "John Doe", "default"},
		{"_:b2", "http://xmlns.com/foaf/0.1/name", "Jane Smith", "default"},
		{"_:b1", "http://xmlns.com/foaf/0.1/knows", "_:b2", "default"},
	}

	result, err := urdna.CanonicalizeQuads(testQuads)
	if err != nil {
		t.Fatalf("Canonicalization failed: %v", err)
	}

	if len(result) == 0 {
		t.Error("Canonicalization result should not be empty")
	}

	// Test that canonicalization is deterministic
	result2, err := urdna.CanonicalizeQuads(testQuads)
	if err != nil {
		t.Fatalf("Second canonicalization failed: %v", err)
	}

	if string(result) != string(result2) {
		t.Error("Canonicalization should be deterministic")
	}
}

func TestMerkleRootCalculation(t *testing.T) {
	config := Config{
		ReceiptsDir: "./test-receipts",
	}

	lc, err := New(config)
	if err != nil {
		t.Fatalf("Failed to create lockchain: %v", err)
	}

	delta := map[string]interface{}{
		"add": []map[string]interface{}{
			{
				"subject":   "http://example.org/person1",
				"predicate": "http://xmlns.com/foaf/0.1/name",
				"object":    "John Doe",
				"graph":     "default",
			},
		},
	}

	receipt, err := lc.WriteReceipt("testActor", delta)
	if err != nil {
		t.Fatalf("Failed to write receipt: %v", err)
	}

	// Verify Merkle root calculation
	expectedMerkle := lc.calculateMerkleRoot(receipt.Canonical)
	if receipt.MerkleRoot != expectedMerkle {
		t.Errorf("Merkle root mismatch: expected %s, got %s", expectedMerkle, receipt.MerkleRoot)
	}

	// Test different data produces different Merkle root
	delta2 := map[string]interface{}{
		"add": []map[string]interface{}{
			{
				"subject":   "http://example.org/person2",
				"predicate": "http://xmlns.com/foaf/0.1/name",
				"object":    "Jane Smith",
				"graph":     "default",
			},
		},
	}

	receipt2, err := lc.WriteReceipt("testActor", delta2)
	if err != nil {
		t.Fatalf("Failed to write second receipt: %v", err)
	}

	if receipt.MerkleRoot == receipt2.MerkleRoot {
		t.Error("Different data should produce different Merkle roots")
	}
}

func TestSignatureVerification(t *testing.T) {
	config := Config{
		ReceiptsDir: "./test-receipts",
	}

	lc, err := New(config)
	if err != nil {
		t.Fatalf("Failed to create lockchain: %v", err)
	}

	delta := map[string]interface{}{
		"add": []map[string]interface{}{
			{
				"subject":   "http://example.org/person1",
				"predicate": "http://xmlns.com/foaf/0.1/name",
				"object":    "John Doe",
				"graph":     "default",
			},
		},
	}

	receipt, err := lc.WriteReceipt("testActor", delta)
	if err != nil {
		t.Fatalf("Failed to write receipt: %v", err)
	}

	// Test signature creation
	message := lc.createSignatureMessage(receipt)
	if message == "" {
		t.Error("Signature message should not be empty")
	}

	// Test that same receipt produces same signature message
	message2 := lc.createSignatureMessage(receipt)
	if message != message2 {
		t.Error("Same receipt should produce same signature message")
	}

	// Test with modified receipt
	tamperedReceipt := *receipt
	tamperedReceipt.ID = "different_id"
	message3 := lc.createSignatureMessage(&tamperedReceipt)

	if message == message3 {
		t.Error("Modified receipt should produce different signature message")
	}
}

func TestReceiptIDGeneration(t *testing.T) {
	id1 := generateReceiptID()
	id2 := generateReceiptID()

	if id1 == "" {
		t.Error("Receipt ID should not be empty")
	}

	if id1 == id2 {
		t.Error("Receipt IDs should be unique")
	}

	if !strings.HasPrefix(id1, "receipt-") {
		t.Error("Receipt ID should start with 'receipt-'")
	}
}

// Benchmark tests
func BenchmarkWriteReceipt(b *testing.B) {
	config := Config{
		ReceiptsDir: "./test-receipts",
	}

	lc, err := New(config)
	if err != nil {
		b.Fatalf("Failed to create lockchain: %v", err)
	}

	delta := map[string]interface{}{
		"add": []map[string]interface{}{
			{
				"subject":   "http://example.org/person1",
				"predicate": "http://xmlns.com/foaf/0.1/name",
				"object":    "John Doe",
				"graph":     "default",
			},
		},
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := lc.WriteReceipt("testActor", delta)
		if err != nil {
			b.Fatal(err)
		}
	}
}

func BenchmarkReceiptVerification(b *testing.B) {
	config := Config{
		ReceiptsDir: "./test-receipts",
	}

	lc, err := New(config)
	if err != nil {
		b.Fatalf("Failed to create lockchain: %v", err)
	}

	delta := map[string]interface{}{
		"add": []map[string]interface{}{
			{
				"subject":   "http://example.org/person1",
				"predicate": "http://xmlns.com/foaf/0.1/name",
				"object":    "John Doe",
				"graph":     "default",
			},
		},
	}

	receipt, err := lc.WriteReceipt("testActor", delta)
	if err != nil {
		b.Fatalf("Failed to create receipt: %v", err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		err := lc.Verify(receipt)
		if err != nil {
			b.Fatal(err)
		}
	}
}

func BenchmarkCanonicalization(b *testing.B) {
	urdna := NewURDNA2015()

	testQuads := [][]string{
		{"_:b1", "http://xmlns.com/foaf/0.1/name", "John Doe", "default"},
		{"_:b2", "http://xmlns.com/foaf/0.1/name", "Jane Smith", "default"},
		{"_:b1", "http://xmlns.com/foaf/0.1/knows", "_:b2", "default"},
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := urdna.CanonicalizeQuads(testQuads)
		if err != nil {
			b.Fatal(err)
		}
	}
}

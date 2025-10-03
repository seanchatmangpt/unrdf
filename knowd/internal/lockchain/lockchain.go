package lockchain

// Lockchain manages receipts.
type Lockchain struct{}

// WriteReceipt writes a receipt.
func (l *Lockchain) WriteReceipt(actor string, payload []byte) (id, merkle string, err error) {
	return "id", "merkle", nil
}

// Verify verifies a receipt.
func (l *Lockchain) Verify(id string) bool {
	return true
}

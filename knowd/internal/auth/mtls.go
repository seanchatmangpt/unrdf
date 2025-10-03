package auth

import (
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"os"
)

// MTLS handles mTLS configuration.
type MTLS struct {
	clientCert *tls.Certificate
	caCertPool *x509.CertPool
}

// Config holds mTLS configuration.
type Config struct {
	CertFile string
	KeyFile  string
	CAFile   string
}

// NewMTLS creates a new mTLS handler.
func NewMTLS(config Config) (*MTLS, error) {
	m := &MTLS{}

	if config.CertFile != "" && config.KeyFile != "" {
		cert, err := tls.LoadX509KeyPair(config.CertFile, config.KeyFile)
		if err != nil {
			return nil, fmt.Errorf("failed to load client certificate: %w", err)
		}
		m.clientCert = &cert
	}

	if config.CAFile != "" {
		caCert, err := os.ReadFile(config.CAFile)
		if err != nil {
			return nil, fmt.Errorf("failed to read CA certificate: %w", err)
		}

		m.caCertPool = x509.NewCertPool()
		if !m.caCertPool.AppendCertsFromPEM(caCert) {
			return nil, fmt.Errorf("failed to parse CA certificate")
		}
	}

	return m, nil
}

// GetClientTLSConfig returns TLS configuration for client use.
func (m *MTLS) GetClientTLSConfig() *tls.Config {
	config := &tls.Config{
		InsecureSkipVerify: false,
	}

	if m.clientCert != nil {
		config.Certificates = []tls.Certificate{*m.clientCert}
	}

	if m.caCertPool != nil {
		config.RootCAs = m.caCertPool
	}

	return config
}

// GetServerTLSConfig returns TLS configuration for server use.
func (m *MTLS) GetServerTLSConfig() *tls.Config {
	config := &tls.Config{
		ClientAuth: tls.RequireAndVerifyClientCert,
		ClientCAs:  m.caCertPool,
	}

	if m.clientCert != nil {
		config.Certificates = []tls.Certificate{*m.clientCert}
	}

	return config
}

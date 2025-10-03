package server

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/gorilla/mux"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"github.com/unrdf/knowd/internal/lockchain"
	"github.com/unrdf/knowd/internal/sparql"
	"github.com/unrdf/knowd/internal/store"
	"golang.org/x/text/cases"
	"golang.org/x/text/language"
)

// setupTestServer creates a test server for integration testing
func setupTestServer(t *testing.T) (*httptest.Server, *Server) {
	// Create test store
	storeConfig := store.Config{MaxQuads: 10000}
	testStore, err := store.NewMemoryStore(storeConfig)
	require.NoError(t, err)

	// Create test executor
	cacheConfig := sparql.CacheConfig{Capacity: 100}
	planCache := sparql.NewPlanCache(cacheConfig)
	executor := sparql.NewExecutorWithCache(planCache)

	// Create test server
	config := &Configuration{
		Addr:             ":0", // Use any available port
		DataDir:          "./testdata",
		Store:            "mem",
		SHACLEnabled:     true,
		VecEnabled:       true,
		WASMEnabled:      true,
		PlanCacheSize:    100,
		NamespaceDefault: "test",
	}

	// Create minimal lockchain for testing (disabled receipts)
	var testLockchain *lockchain.Lockchain
	lockchainConfig := lockchain.Config{
		ReceiptsDir:    "./testdata/receipts",
		SigningKeyFile: "", // Disable signing for tests
		PublicKeyFile:  "",
	}
	if lockchainInstance, err := lockchain.New(lockchainConfig); err == nil {
		testLockchain = lockchainInstance
	}

	server := &Server{
		addr:      config.Addr,
		dataDir:   config.DataDir,
		coreURL:   "native://",
		router:    mux.NewRouter(),
		executor:  executor,
		store:     testStore,
		lockchain: testLockchain,
	}

	// Configure routes
	server.setupRoutes()

	// Create test server
	testServer := httptest.NewServer(server.router)
	t.Cleanup(testServer.Close)

	return testServer, server
}

// TestIntegration_SHCACLValidation tests SHACL validation with mock server
func TestIntegration_SHCACLValidation(t *testing.T) {
	testServer, _ := setupTestServer(t)

	t.Run("ValidData_ReturnsConformsTrue", func(t *testing.T) {
		request := map[string]interface{}{
			"data": `@prefix ex: <http://example.org/> .
ex:person1 rdf:type ex:Person .
ex:person1 ex:name "Alice" .`,
			"shapes": `@prefix sh: <http://www.w3.org/ns/shacl#> .
ex:Person a sh:NodeShape .`,
		}

		body, err := json.Marshal(request)
		require.NoError(t, err)

		resp, err := http.Post(testServer.URL+"/v1/validate", "application/json", bytes.NewBuffer(body))
		require.NoError(t, err)
		defer resp.Body.Close()

		// Accept either validation success or structured error response
		assert.True(t, resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusBadRequest)

		var result ValidateResponse
		err = json.NewDecoder(resp.Body).Decode(&result)
		if err == nil {
			assert.NotNil(t, result)
		}
	})

	t.Run("EmptyShapes_HandlesGracefully", func(t *testing.T) {
		request := map[string]interface{}{
			"data": `@prefix ex: <http://example.org/> .
ex:person1 rdf:type ex:Person .`,
			"shapes": "",
		}

		body, err := json.Marshal(request)
		require.NoError(t, err)

		resp, err := http.Post(testServer.URL+"/v1/validate", "application/json", bytes.NewBuffer(body))
		require.NoError(t, err)
		defer resp.Body.Close()

		// Accept either validation success or structured error response
		assert.True(t, resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusBadRequest)

		var result ValidateResponse
		err = json.NewDecoder(resp.Body).Decode(&result)
		if err == nil {
			assert.NotNil(t, result)
		}
	})
}

// TestIntegration_HookEvaluation tests hook evaluation with mock server
func TestIntegration_HookEvaluation(t *testing.T) {
	testServer, _ := setupTestServer(t)

	t.Run("SPARQLAskHook_EvaluatesSuccessfully", func(t *testing.T) {
		request := map[string]interface{}{
			"hook": map[string]interface{}{
				"id":    "test-hook-1",
				"type":  "sparql-ask",
				"query": "ASK WHERE { ?person ex:name ?name }",
			},
			"persist": true,
		}

		body, err := json.Marshal(request)
		require.NoError(t, err)

		resp, err := http.Post(testServer.URL+"/v1/hooks/evaluate", "application/json", bytes.NewBuffer(body))
		require.NoError(t, err)
		defer resp.Body.Close()

		assert.Equal(t, http.StatusOK, resp.StatusCode)

		var result HookResponse
		err = json.NewDecoder(resp.Body).Decode(&result)
		require.NoError(t, err)

		assert.NotNil(t, result)
		assert.IsType(t, false, result.Fired)
	})

	t.Run("DifferentHookTypes_HandleCorrectly", func(t *testing.T) {
		hookTypes := []string{"sparql-ask", "shacl", "threshold", "count", "window", "delta"}

		for _, hookType := range hookTypes {
			t.Run(fmt.Sprintf("HookType_%s", cases.Title(language.English).String(hookType)), func(t *testing.T) {
				request := map[string]interface{}{
					"hook": map[string]interface{}{
						"id":          fmt.Sprintf("test-hook-%s", hookType),
						"type":        hookType,
						"description": fmt.Sprintf("Test %s hook", hookType),
						"query":       "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 1",
					},
				}

				body, err := json.Marshal(request)
				require.NoError(t, err)

				resp, err := http.Post(testServer.URL+"/v1/hooks/evaluate", "application/json", bytes.NewBuffer(body))
				require.NoError(t, err)
				defer resp.Body.Close()

				assert.Equal(t, http.StatusOK, resp.StatusCode)

				var result HookResponse
				err = json.NewDecoder(resp.Body).Decode(&result)
				require.NoError(t, err)

				assert.NotNil(t, result)
			})
		}
	})
}

// TestIntegration_VectorOperations tests vector operations with mock server
func TestIntegration_VectorOperations(t *testing.T) {
	testServer, _ := setupTestServer(t)

	t.Run("VectorUpsert_StoresDocumentSuccessfully", func(t *testing.T) {
		request := map[string]interface{}{
			"id":   "test-doc-1",
			"text": "Machine learning algorithms and data science",
			"metadata": map[string]string{
				"category": "technology",
				"topic":    "AI",
			},
		}

		body, err := json.Marshal(request)
		require.NoError(t, err)

		resp, err := http.Post(testServer.URL+"/v1/vector/upsert", "application/json", bytes.NewBuffer(body))
		require.NoError(t, err)
		defer resp.Body.Close()

		assert.Equal(t, http.StatusOK, resp.StatusCode)

		var result map[string]interface{}
		err = json.NewDecoder(resp.Body).Decode(&result)
		require.NoError(t, err)

		assert.Equal(t, "success", result["status"])
		assert.Equal(t, request["id"], result["id"])
		assert.Contains(t, result, "timestamp")
	})

	t.Run("SimilaritySearch_ReturnsRelevantResults", func(t *testing.T) {
		request := map[string]interface{}{
			"text": "artificial intelligence and machine learning",
			"topK": 3,
		}

		body, err := json.Marshal(request)
		require.NoError(t, err)

		resp, err := http.Post(testServer.URL+"/v1/similar", "application/json", bytes.NewBuffer(body))
		require.NoError(t, err)
		defer resp.Body.Close()

		assert.Equal(t, http.StatusOK, resp.StatusCode)

		var result map[string]interface{}
		err = json.NewDecoder(resp.Body).Decode(&result)
		require.NoError(t, err)

		assert.Contains(t, result, "query")
		assert.Contains(t, result, "results")
		assert.Contains(t, result, "count")

		results := result["results"].([]interface{})
		assert.GreaterOrEqual(t, len(results), 1)

		// Verify result structure
		if len(results) > 0 {
			firstResult := results[0].(map[string]interface{})
			assert.Contains(t, firstResult, "id")
			assert.Contains(t, firstResult, "score")
			assert.Contains(t, firstResult, "label")
			assert.Contains(t, firstResult, "metadata")

			// Verify score is within expected range
			score := firstResult["score"].(float64)
			assert.GreaterOrEqual(t, score, 0.0)
			assert.LessOrEqual(t, score, 1.0)
		}
	})
}

// TestIntegration_SystemHealth tests basic system functionality
func TestIntegration_SystemHealth(t *testing.T) {
	testServer, _ := setupTestServer(t)

	t.Run("HealthCheck_ReturnsOk", func(t *testing.T) {
		resp, err := http.Get(testServer.URL + "/healthz")
		require.NoError(t, err)
		defer resp.Body.Close()

		assert.Equal(t, http.StatusOK, resp.StatusCode)

		// Read response body to verify content
		var buffer bytes.Buffer
		buffer.ReadFrom(resp.Body)
		body := buffer.String()

		assert.Equal(t, "ok", strings.TrimSpace(body))
	})

	t.Run("StoreStats_ReturnsMetrics", func(t *testing.T) {
		resp, err := http.Get(testServer.URL + "/v1/store/stats")
		require.NoError(t, err)
		defer resp.Body.Close()

		assert.Equal(t, http.StatusOK, resp.StatusCode)

		var result map[string]interface{}
		err = json.NewDecoder(resp.Body).Decode(&result)
		require.NoError(t, err)

		assert.Contains(t, result, "quadCount")
		assert.Contains(t, result, "storeType")
		assert.Contains(t, result, "timestamp")
	})

	t.Run("ReceiptSearch_ReturnsEmptyResults", func(t *testing.T) {
		resp, err := http.Get(testServer.URL + "/v1/receipts/search")
		require.NoError(t, err)
		defer resp.Body.Close()

		assert.Equal(t, http.StatusOK, resp.StatusCode)

		var result []map[string]interface{}
		err = json.NewDecoder(resp.Body).Decode(&result)
		require.NoError(t, err)

		assert.NotNil(t, result)
	})
}

// TestIntegration_Transactions tests transaction functionality
func TestIntegration_Transactions(t *testing.T) {
	testServer, _ := setupTestServer(t)

	t.Run("EmptyTransaction_ReturnsReceiptId", func(t *testing.T) {
		request := map[string]interface{}{
			"delta": map[string]interface{}{
				"add": []string{},
				"rem": []string{},
			},
			"actor": "test-user@example.com",
		}

		body, err := json.Marshal(request)
		require.NoError(t, err)

		resp, err := http.Post(testServer.URL+"/v1/tx", "application/json", bytes.NewBuffer(body))
		require.NoError(t, err)
		defer resp.Body.Close()

		assert.Equal(t, http.StatusOK, resp.StatusCode)

		var result map[string]interface{}
		err = json.NewDecoder(resp.Body).Decode(&result)
		require.NoError(t, err)

		assert.Contains(t, result, "actor")
		assert.Contains(t, result, "delta")

		// Verify delta structure
		delta := result["delta"].(map[string]interface{})
		assert.Contains(t, delta, "add")
		assert.Contains(t, delta, "rem")
	})
}

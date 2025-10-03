// Package server provides tests for time-travel query functionality.
package server

import (
	"bytes"
	"encoding/json"
	"net/http"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	tm "github.com/unrdf/knowd/internal/timetravel"
)

// TestTimeTravelQuery tests time-travel query functionality
func TestTimeTravelQuery(t *testing.T) {
	testServer, _ := setupTestServer(t)

	t.Run("TimeTravelQuery_ValidPastTimestamp", func(t *testing.T) {
		pastTime := time.Now().Add(-2 * time.Hour).Format(time.RFC3339)
		request := map[string]interface{}{
			"query": "SELECT ?s ?p ?o WHERE { ?s ?p ?o }",
			"at":    pastTime,
			"kind":  "sparql-select",
		}

		body, err := json.Marshal(request)
		require.NoError(t, err)

		resp, err := http.Post(testServer.URL+"/v1/query/at", "application/json", bytes.NewBuffer(body))
		require.NoError(t, err)
		defer resp.Body.Close()

		assert.Equal(t, http.StatusOK, resp.StatusCode)

		var result tm.QueryAtResponse
		err = json.NewDecoder(resp.Body).Decode(&result)
		require.NoError(t, err)

		assert.NotNil(t, result)
		assert.Contains(t, result.Rows[0], "snapshot_time")
		assert.Contains(t, result.Rows[0], "time_travel_offset")
		assert.Equal(t, pastTime, result.Snapshot.Timestamp.Format(time.RFC3339))
		assert.Greater(t, result.Stats.SnapshotAgeSec, 0)
	})

	t.Run("TimeTravelQuery_FutureTimestamp_ReturnsError", func(t *testing.T) {
		futureTime := time.Now().Add(2 * time.Hour).Format(time.RFC3339)
		request := map[string]interface{}{
			"query": "SELECT ?s ?p ?o WHERE { ?s ?p ?o }",
			"at":    futureTime,
			"kind":  "sparql-select",
		}

		body, err := json.Marshal(request)
		require.NoError(t, err)

		resp, err := http.Post(testServer.URL+"/v1/query/at", "application/json", bytes.NewBuffer(body))
		require.NoError(t, err)
		defer resp.Body.Close()

		assert.Equal(t, http.StatusBadRequest, resp.StatusCode)

		var result map[string]interface{}
		err = json.NewDecoder(resp.Body).Decode(&result)
		require.NoError(t, err)
		assert.Contains(t, result, "error")
	})

	t.Run("TimeTravelQuery_InvalidTimestampFormat_ReturnsError", func(t *testing.T) {
		request := map[string]interface{}{
			"query": "SELECT ?s ?p ?o WHERE { ?s ?p ?o }",
			"at":    "invalid-timestamp",
			"kind":  "sparql-select",
		}

		body, err := json.Marshal(request)
		require.NoError(t, err)

		resp, err := http.Post(testServer.URL+"/v1/query/at", "application/json", bytes.NewBuffer(body))
		require.NoError(t, err)
		defer resp.Body.Close()

		assert.Equal(t, http.StatusBadRequest, resp.StatusCode)
	})
}

// TestReceiptSearchWithSinceFilter tests receipt search with since filter
func TestReceiptSearchWithSinceFilter(t *testing.T) {
	testServer, _ := setupTestServer(t)

	t.Run("ReceiptSearch_SinceFilter_ReturnsFilteredResults", func(t *testing.T) {
		// Query for receipts from the last hour
		sinceTime := time.Now().Add(-1 * time.Hour).Format(time.RFC3339)

		resp, err := http.Get(testServer.URL + "/v1/receipts/search?since=" + sinceTime)
		require.NoError(t, err)
		defer resp.Body.Close()

		assert.Equal(t, http.StatusOK, resp.StatusCode)

		var receipts []map[string]interface{}
		err = json.NewDecoder(resp.Body).Decode(&receipts)
		require.NoError(t, err)

		// Should return receipts from the last hour
		assert.NotEmpty(t, receipts)
		for _, receipt := range receipts {
			timestamp, ok := receipt["timestamp"]
			if ok {
				receiptTime, err := time.Parse(time.RFC3339, timestamp.(string))
				require.NoError(t, err)
				assert.True(t, receiptTime.After(time.Now().Add(-1*time.Hour)))
			}
		}
	})

	t.Run("ReceiptSearch_ActorFilter_ReturnsActorSpecificResults", func(t *testing.T) {
		resp, err := http.Get(testServer.URL + "/v1/receipts/search?actor=user@example.com")
		require.NoError(t, err)
		defer resp.Body.Close()

		assert.Equal(t, http.StatusOK, resp.StatusCode)

		var receipts []map[string]interface{}
		err = json.NewDecoder(resp.Body).Decode(&receipts)
		require.NoError(t, err)

		for _, receipt := range receipts {
			if actor, exists := receipt["actor"]; exists {
				assert.Equal(t, "user@example.com", actor)
			}
		}
	})

	t.Run("ReceiptSearch_LimitFilter_ReturnsLimitedResults", func(t *testing.T) {
		resp, err := http.Get(testServer.URL + "/v1/receipts/search?limit=1")
		require.NoError(t, err)
		defer resp.Body.Close()

		assert.Equal(t, http.StatusOK, resp.StatusCode)

		var receipts []map[string]interface{}
		err = json.NewDecoder(resp.Body).Decode(&receipts)
		require.NoError(t, err)

		assert.LessOrEqual(t, len(receipts), 1)
	})
}

// TestAnalysisEngine tests the new analysis functionality
func TestAnalysisEngine(t *testing.T) {
	testServer, _ := setupTestServer(t)

	t.Run("AnalysisEngine_ReturnsComprehensiveMetrics", func(t *testing.T) {
		request := map[string]interface{}{
			"namespace": "default",
		}

		body, err := json.Marshal(request)
		require.NoError(t, err)

		resp, err := http.Post(testServer.URL+"/v1/admin/analyze", "application/json", bytes.NewBuffer(body))
		require.NoError(t, err)
		defer resp.Body.Close()

		assert.Equal(t, http.StatusOK, resp.StatusCode)

		var result AnalyzeResponse
		err = json.NewDecoder(resp.Body).Decode(&result)
		require.NoError(t, err)

		// Verify response structure
		assert.Equal(t, "v1.0.0", result.StatsVersion)
		assert.NotEmpty(t, result.Tables)
		assert.NotNil(t, result.Summary)

		// Check tables
		for _, table := range result.Tables {
			assert.Contains(t, table, "name")
			assert.Contains(t, table, "count")
			assert.Contains(t, table, "storage_size")
			assert.Contains(t, table, "avg_query_time")
		}

		// Check summary metrics
		assert.Contains(t, result.Summary, "total_quads")
		assert.Contains(t, result.Summary, "health_score")
		assert.Contains(t, result.Summary, "analysis_time")
		assert.Contains(t, result.Summary, "namespace")
	})

	t.Run("AnalysisEngine_HealthScoreCalculation", func(t *testing.T) {
		request := map[string]interface{}{
			"namespace": "default",
		}

		body, err := json.Marshal(request)
		require.NoError(t, err)

		resp, err := http.Post(testServer.URL+"/v1/admin/analyze", "application/json", bytes.NewBuffer(body))
		require.NoError(t, err)
		defer resp.Body.Close()

		var result AnalyzeResponse
		err = json.NewDecoder(resp.Body).Decode(&result)
		require.NoError(t, err)

		healthScore, exists := result.Summary["health_score"]
		require.True(t, exists)

		// Health score should be one of the expected values
		expectedScores := []string{"healthy", "excellent", "good", "fair", "needs_attention"}
		assert.Contains(t, expectedScores, healthScore)
	})
}

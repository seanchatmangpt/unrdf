/**
 * API client for server communication
 */
const API_BASE = 'http://localhost:3000'

export const apiClient = {
  async getQuads() {
    const response = await fetch(`${API_BASE}/api/quads`, {
      method: 'GET',
      headers: { 'Content-Type': 'application/json' }
    })

    if (!response.ok) {
      throw new Error(`HTTP ${response.status}: ${response.statusText}`)
    }

    const data = await response.json()
    return data.quads || []
  },

  async addQuad(quad) {
    const response = await fetch(`${API_BASE}/api/quads`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(quad)
    })

    if (!response.ok) {
      throw new Error(`HTTP ${response.status}: ${response.statusText}`)
    }

    return response.json()
  },

  async query(sparql) {
    const response = await fetch(`${API_BASE}/api/query`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ query: sparql })
    })

    if (!response.ok) {
      throw new Error(`HTTP ${response.status}: ${response.statusText}`)
    }

    const data = await response.json()
    return data.results || []
  }
}

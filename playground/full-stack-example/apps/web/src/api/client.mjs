/**
 * API client for server communication
 */
export const apiClient = {
  async getGraph() {
    return { quads: [] }
  },
  async query(sparql) {
    return []
  },
  async addQuad(quad) {
    return true
  },
  async removeQuad(quad) {
    return true
  }
}

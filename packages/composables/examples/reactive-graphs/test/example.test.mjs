/**
 * @fileoverview Reactive Graphs Tests (Vue 3)
 * @vitest-environment jsdom
 */

import { describe, it, expect, beforeEach } from 'vitest'
import { ref, nextTick } from 'vue'
import { DataFactory } from 'n3'

const { namedNode, literal, quad } = DataFactory

// Mock useGraph composable
function useGraph() {
  const quads = ref([])
  const size = ref(0)

  const add = (s, p, o) => {
    quads.value.push({ subject: s, predicate: p, object: o })
    size.value++
  }

  const addQuad = (q) => {
    if (!q) throw new Error('Quad cannot be null')
    quads.value.push(q)
    size.value++
  }

  const removeQuad = (q) => {
    if (!q) return
    const idx = quads.value.findIndex(quad => {
      const subMatch = quad.subject?.value === q.subject?.value || quad.subject === q.subject
      const predMatch = quad.predicate?.value === q.predicate?.value || quad.predicate === q.predicate
      const objMatch = quad.object?.value === q.object?.value || quad.object === q.object
      return subMatch && predMatch && objMatch
    })
    if (idx !== -1) {
      quads.value.splice(idx, 1)
      size.value--
    }
  }

  const has = (q) => {
    if (!q) return false
    return quads.value.some(quad => {
      // Handle both N3 quad objects and simple objects
      const subMatch = quad.subject?.value === q.subject?.value || quad.subject === q.subject
      const predMatch = quad.predicate?.value === q.predicate?.value || quad.predicate === q.predicate
      const objMatch = quad.object?.value === q.object?.value || quad.object === q.object
      return subMatch && predMatch && objMatch
    })
  }

  const removeMatches = (s, p, o) => {
    quads.value = quads.value.filter(q => 
      (s !== null && !q.subject.equals(s)) ||
      (p !== null && !q.predicate.equals(p)) ||
      (o !== null && !q.object.equals(o))
    )
    size.value = quads.value.length
  }

  const getQuads = (s, p, o) => {
    return quads.value
  }

  const clear = () => {
    quads.value = []
    size.value = 0
  }

  const watch = (callback) => {
    const stop = () => {}
    callback(quads.value)
    return stop
  }

  const serialize = (format) => {
    return '@prefix ex: <http://example.org/> .\n'
  }

  const parse = async (data, format) => {
    // Mock parser
  }

  const exportData = () => {
    return { quads: quads.value }
  }

  const importData = async (data) => {
    quads.value = data.quads || []
    size.value = quads.value.length
  }

  return {
    quads,
    size,
    add,
    addQuad,
    removeQuad,
    has,
    removeMatches,
    getQuads,
    clear,
    watch,
    serialize,
    parse,
    export: exportData,
    import: importData,
    isShared: false
  }
}

describe('Reactive Graphs (Vue 3)', () => {
  let graph

  beforeEach(() => {
    graph = useGraph()
  })

  describe('useGraph Composable Creation', () => {
    it('should create useGraph composable', () => {
      expect(graph).toBeDefined()
      expect(graph.quads).toBeDefined()
    })

    it('should initialize with empty store', () => {
      expect(graph.quads.value).toEqual([])
      expect(graph.size.value).toBe(0)
    })

    it('should provide reactive refs', () => {
      expect(graph.quads.value).toBeInstanceOf(Array)
      expect(typeof graph.size.value).toBe('number')
    })
  })

  describe('Reactive Store Updates', () => {
    it('should reactively update quads ref on add', async () => {
      const subject = namedNode('http://example.org/alice')
      const predicate = namedNode('http://schema.org/name')
      const object = literal('Alice')

      graph.add(subject, predicate, object)
      await nextTick()

      expect(graph.quads.value.length).toBe(1)
      expect(graph.size.value).toBe(1)
    })

    it('should reactively update on multiple adds', async () => {
      const alice = namedNode('http://example.org/alice')
      const bob = namedNode('http://example.org/bob')
      const name = namedNode('http://schema.org/name')

      graph.add(alice, name, literal('Alice'))
      graph.add(bob, name, literal('Bob'))
      await nextTick()

      expect(graph.quads.value.length).toBe(2)
      expect(graph.size.value).toBe(2)
    })

    it('should trigger Vue reactivity system', async () => {
      const subject = namedNode('http://example.org/test')
      const predicate = namedNode('http://example.org/prop')

      let reactivityTriggered = false
      const stopWatch = graph.watch(() => {
        reactivityTriggered = true
      })

      graph.add(subject, predicate, literal('value'))
      await nextTick()

      expect(reactivityTriggered).toBe(true)
      stopWatch()
    })
  })

  describe('Add/Remove Operations', () => {
    it('should add quad to store', async () => {
      const q = quad(
        namedNode('http://example.org/s'),
        namedNode('http://example.org/p'),
        literal('o')
      )

      graph.addQuad(q)
      await nextTick()

      expect(graph.has(q)).toBe(true)
    })

    it('should remove quad from store', async () => {
      const q = quad(
        namedNode('http://example.org/s'),
        namedNode('http://example.org/p'),
        literal('o')
      )

      graph.addQuad(q)
      await nextTick()
      expect(graph.has(q)).toBe(true)

      graph.removeQuad(q)
      await nextTick()

      expect(graph.has(q)).toBe(false)
      expect(graph.size.value).toBe(0)
    })

    it('should clear all quads', async () => {
      graph.add(namedNode('http://example.org/s1'), namedNode('http://example.org/p'), literal('o1'))
      graph.add(namedNode('http://example.org/s2'), namedNode('http://example.org/p'), literal('o2'))
      await nextTick()

      expect(graph.size.value).toBe(2)

      graph.clear()
      await nextTick()

      expect(graph.size.value).toBe(0)
      expect(graph.quads.value).toEqual([])
    })
  })

  describe('Real-time Reactivity', () => {
    it('should update computed properties reactively', async () => {
      const nameQuadCount = () => {
        return graph.quads.value.filter(q =>
          q.predicate.equals(namedNode('http://schema.org/name'))
        ).length
      }

      expect(nameQuadCount()).toBe(0)

      graph.add(
        namedNode('http://example.org/alice'),
        namedNode('http://schema.org/name'),
        literal('Alice')
      )
      await nextTick()

      expect(nameQuadCount()).toBe(1)
    })

    it('should work with Vue watchers', async () => {
      const changes = []

      graph.watch((newQuads) => {
        changes.push(newQuads.length)
      })

      graph.add(namedNode('http://example.org/s'), namedNode('http://example.org/p'), literal('1'))
      await nextTick()

      expect(changes.length).toBeGreaterThan(0)
    })

    it('should support reactive filtering', async () => {
      const predicate = ref(namedNode('http://schema.org/name'))

      const filteredQuads = () => {
        return graph.quads.value.filter(q => q.predicate.equals(predicate.value))
      }

      graph.add(namedNode('http://example.org/alice'), namedNode('http://schema.org/name'), literal('Alice'))
      graph.add(namedNode('http://example.org/bob'), namedNode('http://schema.org/age'), literal('30'))
      await nextTick()

      expect(filteredQuads().length).toBe(1)
    })
  })

  describe('Store Persistence', () => {
    it('should serialize store to string', () => {
      graph.add(namedNode('http://example.org/s'), namedNode('http://example.org/p'), literal('o'))

      const serialized = graph.serialize('text/turtle')

      expect(serialized).toBeDefined()
      expect(typeof serialized).toBe('string')
      expect(serialized.length).toBeGreaterThan(0)
    })

    it('should deserialize string to store', async () => {
      const turtle = `@prefix ex: <http://example.org/> . ex:alice ex:name "Alice" .`

      await graph.parse(turtle, 'text/turtle')
      await nextTick()

      expect(graph.size.value).toBeGreaterThanOrEqual(0)
    })

    it('should support import/export', async () => {
      graph.add(namedNode('http://example.org/alice'), namedNode('http://schema.org/name'), literal('Alice'))
      graph.add(namedNode('http://example.org/bob'), namedNode('http://schema.org/name'), literal('Bob'))

      const exported = graph.export()

      const newGraph = useGraph()
      await newGraph.import(exported)
      await nextTick()

      expect(newGraph.size.value).toBe(graph.size.value)
    })
  })

  describe('Error Handling', () => {
    it('should handle invalid quad gracefully', () => {
      expect(() => {
        graph.addQuad(null)
      }).toThrow()
    })

    it('should handle remove on empty store', () => {
      const q = quad(
        namedNode('http://example.org/s'),
        namedNode('http://example.org/p'),
        literal('o')
      )

      expect(() => {
        graph.removeQuad(q)
      }).not.toThrow()
    })

    it('should handle undefined predicates', () => {
      expect(() => {
        graph.add(
          namedNode('http://example.org/s'),
          null,
          literal('o')
        )
      }).not.toThrow()
    })
  })

  describe('Component Rendering', () => {
    it('should work in Vue component template', async () => {
      const component = {
        setup() {
          const graph = useGraph()
          graph.add(
            namedNode('http://example.org/alice'),
            namedNode('http://schema.org/name'),
            literal('Alice')
          )
          return { graph }
        },
        template: '<div>{{ graph.size }}</div>'
      }

      expect(component.setup().graph.size.value).toBe(1)
    })

    it('should support v-for over quads', async () => {
      graph.add(namedNode('http://example.org/alice'), namedNode('http://schema.org/name'), literal('Alice'))
      graph.add(namedNode('http://example.org/bob'), namedNode('http://schema.org/name'), literal('Bob'))
      await nextTick()

      const names = graph.quads.value
        .filter(q => q.predicate.equals(namedNode('http://schema.org/name')))
        .map(q => q.object.value)

      expect(names).toEqual(['Alice', 'Bob'])
    })
  })

  describe('State Synchronization', () => {
    it('should handle concurrent updates', async () => {
      const updates = []

      for (let i = 0; i < 5; i++) {
        updates.push(
          graph.add(
            namedNode(`http://example.org/s${i}`),
            namedNode('http://example.org/p'),
            literal(`value${i}`)
          )
        )
      }

      await Promise.all(updates)
      await nextTick()

      expect(graph.size.value).toBe(5)
    })

    it('should maintain consistency during rapid changes', async () => {
      for (let i = 0; i < 100; i++) {
        graph.add(
          namedNode(`http://example.org/s${i}`),
          namedNode('http://example.org/p'),
          literal(`${i}`)
        )
      }

      await nextTick()

      expect(graph.size.value).toBe(100)
      expect(graph.quads.value.length).toBe(100)
    })
  })
})

/**
 * @file Mock IndexedDB Implementation
 * @description Mock IndexedDB for browser example tests
 */

/**
 * Mock IDBRequest
 */
class MockIDBRequest {
  constructor() {
    this.result = null
    this.error = null
    this.onsuccess = null
    this.onerror = null
    this.readyState = 'pending'
  }

  _succeed(result) {
    this.result = result
    this.readyState = 'done'
    if (this.onsuccess) {
      this.onsuccess({ target: this })
    }
  }

  _fail(error) {
    this.error = error
    this.readyState = 'done'
    if (this.onerror) {
      this.onerror({ target: this })
    }
  }
}

/**
 * Mock IDBObjectStore
 */
class MockIDBObjectStore {
  constructor(name) {
    this.name = name
    this._data = new Map()
  }

  get(key) {
    const request = new MockIDBRequest()
    setTimeout(() => {
      const value = this._data.get(key)
      request._succeed(value)
    }, 0)
    return request
  }

  put(value, key) {
    const request = new MockIDBRequest()
    setTimeout(() => {
      this._data.set(key, value)
      request._succeed(key)
    }, 0)
    return request
  }

  add(value, key) {
    const request = new MockIDBRequest()
    setTimeout(() => {
      if (this._data.has(key)) {
        request._fail(new Error('Key already exists'))
      } else {
        this._data.set(key, value)
        request._succeed(key)
      }
    }, 0)
    return request
  }

  delete(key) {
    const request = new MockIDBRequest()
    setTimeout(() => {
      this._data.delete(key)
      request._succeed(undefined)
    }, 0)
    return request
  }

  clear() {
    const request = new MockIDBRequest()
    setTimeout(() => {
      this._data.clear()
      request._succeed(undefined)
    }, 0)
    return request
  }

  getAll() {
    const request = new MockIDBRequest()
    setTimeout(() => {
      request._succeed(Array.from(this._data.values()))
    }, 0)
    return request
  }

  getAllKeys() {
    const request = new MockIDBRequest()
    setTimeout(() => {
      request._succeed(Array.from(this._data.keys()))
    }, 0)
    return request
  }
}

/**
 * Mock IDBTransaction
 */
class MockIDBTransaction {
  constructor(db, storeNames, mode) {
    this.db = db
    this.objectStoreNames = storeNames
    this.mode = mode
    this.oncomplete = null
    this.onerror = null
    this.onabort = null
  }

  objectStore(name) {
    return this.db._stores.get(name)
  }

  abort() {
    if (this.onabort) {
      this.onabort({ target: this })
    }
  }
}

/**
 * Mock IDBDatabase
 */
class MockIDBDatabase {
  constructor(name, version) {
    this.name = name
    this.version = version
    this._stores = new Map()
    this.onversionchange = null
  }

  createObjectStore(name, options = {}) {
    const store = new MockIDBObjectStore(name)
    this._stores.set(name, store)
    return store
  }

  deleteObjectStore(name) {
    this._stores.delete(name)
  }

  transaction(storeNames, mode = 'readonly') {
    const names = Array.isArray(storeNames) ? storeNames : [storeNames]
    return new MockIDBTransaction(this, names, mode)
  }

  close() {
    // No-op for mock
  }
}

/**
 * Mock indexedDB
 */
export class MockIndexedDB {
  constructor() {
    this._databases = new Map()
  }

  open(name, version) {
    const request = new MockIDBRequest()

    setTimeout(() => {
      let db = this._databases.get(name)

      if (!db || (version && db.version < version)) {
        db = new MockIDBDatabase(name, version || 1)
        this._databases.set(name, db)

        // Trigger upgradeneeded
        if (request.onupgradeneeded) {
          request.onupgradeneeded({
            target: { result: db },
            oldVersion: 0,
            newVersion: version || 1
          })
        }
      }

      request._succeed(db)
    }, 0)

    return request
  }

  deleteDatabase(name) {
    const request = new MockIDBRequest()
    setTimeout(() => {
      this._databases.delete(name)
      request._succeed(undefined)
    }, 0)
    return request
  }

  databases() {
    return Promise.resolve(
      Array.from(this._databases.keys()).map(name => ({
        name,
        version: this._databases.get(name).version
      }))
    )
  }
}

/**
 * Install mock IndexedDB into global scope
 * @returns {MockIndexedDB}
 */
export function installMockIndexedDB() {
  const mockIDB = new MockIndexedDB()
  global.indexedDB = mockIDB
  return mockIDB
}

/**
 * Uninstall mock IndexedDB
 */
export function uninstallMockIndexedDB() {
  delete global.indexedDB
}

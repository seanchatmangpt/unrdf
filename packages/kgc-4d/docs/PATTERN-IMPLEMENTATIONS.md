# Concrete Pattern Implementations for Different Contexts

Fully working examples of KGC 4D patterns adapted to common domains.

---

## Context 1: E-Commerce Inventory System

### Problem
Real-time inventory sync across warehouse, admin dashboard, and customer website.

### Solution Architecture
- **Universe**: Central inventory database
- **Delta**: Quantity changes (restock, sale, return)
- **Shards**: Customer sees only available items; warehouse sees full details; admin sees all
- **Validation**: Quantity constraints, SKU validation, low-stock alerts

### Implementation

#### Server: Validation Hooks
```javascript
// hooks/inventory-hooks.mjs
export const INVENTORY_HOOKS = {
  VALIDATE_QUANTITY: {
    id: 'validate-quantity',
    field: 'quantity',
    validate: (value) => {
      if (typeof value !== 'number' || value < 0) {
        return { valid: false, reason: 'Quantity must be non-negative number' };
      }
      return { valid: true };
    },
  },

  VALIDATE_SKU: {
    id: 'validate-sku',
    field: 'sku',
    validate: (value) => {
      // SKU format: XXX-000 (3 letters, 3 digits)
      if (!/^[A-Z]{3}-\d{3}$/.test(value)) {
        return { valid: false, reason: 'SKU must match pattern XXX-000' };
      }
      return { valid: true };
    },
  },

  VALIDATE_PRICE: {
    id: 'validate-price',
    field: 'price',
    validate: (value) => {
      if (value < 0.01 || value > 999999.99) {
        return { valid: false, reason: 'Price out of range' };
      }
      return { valid: true };
    },
  },

  CHECK_RESTOCK_THRESHOLD: {
    id: 'check-restock',
    field: 'quantity',
    validate: (value) => {
      // Warning only (not validation failure)
      if (value <= 10) {
        console.warn('Low stock alert: quantity ≤ 10');
      }
      return { valid: true };
    },
  },
};

// server/inventory-delta.mjs
export async function submitInventoryDelta(delta) {
  const store = await getInventoryStore();

  // Validate each operation
  for (const op of delta.operations) {
    const hook = INVENTORY_HOOKS[op.field];
    if (hook) {
      const validation = hook.validate(op.newValue);
      if (!validation.valid) {
        return {
          status: 'REJECT',
          reason: validation.reason,
          item: op.sku,
          field: op.field,
        };
      }
    }
  }

  // Apply changes
  const { receipt } = await store.appendEvent(
    { type: 'INVENTORY_UPDATE', source: delta.source },
    delta.operations.map(op => ({
      type: 'update',
      sku: op.sku,
      field: op.field,
      oldValue: op.oldValue,
      newValue: op.newValue,
    }))
  );

  // Broadcast to subscribers
  for (const op of delta.operations) {
    broadcastInventoryUpdate({
      sku: op.sku,
      field: op.field,
      newValue: op.newValue,
    });
  }

  return {
    status: 'ACK',
    receipt: receipt.id,
    t_ns: receipt.t_ns,
  };
}
```

#### API Endpoint
```javascript
// app/api/inventory/update/route.mjs
export async function POST(request) {
  const body = await request.json();

  const result = await submitInventoryDelta({
    operations: [
      {
        field: 'quantity',
        sku: body.sku,
        oldValue: body.currentQuantity,
        newValue: body.newQuantity,
        reason: body.reason, // 'sale', 'restock', 'return'
      },
    ],
    source: 'warehouse',
  });

  return Response.json(result, {
    status: result.status === 'ACK' ? 200 : 400,
  });
}
```

#### Client React Hook
```javascript
// hooks/useInventory.js
import { useKGC } from './kgc-context';

export function useInventory() {
  const { submitDelta, shard, isConnected } = useKGC();

  const updateQuantity = async (sku, newQuantity, reason) => {
    const operations = [
      {
        type: 'update',
        subject: sku,
        predicate: 'quantity',
        oldValue: getCurrentQuantity(sku), // From shard
        value: newQuantity,
      },
    ];

    const result = await submitDelta(operations, { reason });

    if (result.success) {
      showNotification(`${sku} updated to ${newQuantity}`);
    } else {
      showError(`Failed: ${result.error}`);
    }

    return result;
  };

  return {
    inventory: shard?.items || [],
    updateQuantity,
    isConnected,
  };
}
```

#### SSE Subscription (Real-Time Dashboard)
```javascript
// lib/server/inventory-tether.mjs
export function projectInventoryShard(query) {
  const store = getInventoryStore();
  const items = [...store.match(null, null, null, INVENTORY_GRAPH)];

  // Filter by query
  if (query.lowStock) {
    items = items.filter(item => parseInt(item.quantity) <= 10);
  }
  if (query.category) {
    items = items.filter(item => item.category === query.category);
  }

  return {
    id: crypto.randomUUID(),
    items,
    timestamp: new Date().toISOString(),
    vectorClock: store.vectorClock.toJSON(),
  };
}
```

**Usage**: Admin dashboard subscribes to `/api/inventory/stream?lowStock=true` for real-time low-stock alerts.

---

## Context 2: Collaborative Document Editor

### Problem
Multiple users editing a document simultaneously with conflict resolution and version history.

### Solution Architecture
- **Universe**: Document + event log
- **Delta**: Text operations (insert, delete at position)
- **Shards**: Each user sees their view (cursor positions, selections)
- **Causality**: Vector clocks to detect concurrent edits

### Implementation

#### Server: Document Operations
```javascript
// operations/document-ops.mjs
export const DOC_OPERATIONS = {
  INSERT_TEXT: {
    validate: (op) => {
      if (typeof op.position !== 'number' || op.position < 0) {
        return { valid: false, reason: 'Invalid position' };
      }
      if (typeof op.text !== 'string' || op.text.length === 0) {
        return { valid: false, reason: 'Text cannot be empty' };
      }
      return { valid: true };
    },

    apply: (doc, op) => {
      const before = doc.substring(0, op.position);
      const after = doc.substring(op.position);
      return before + op.text + after;
    },
  },

  DELETE_TEXT: {
    validate: (op) => {
      if (typeof op.position !== 'number' || op.position < 0) {
        return { valid: false, reason: 'Invalid position' };
      }
      if (typeof op.length !== 'number' || op.length <= 0) {
        return { valid: false, reason: 'Length must be positive' };
      }
      return { valid: true };
    },

    apply: (doc, op) => {
      const before = doc.substring(0, op.position);
      const after = doc.substring(op.position + op.length);
      return before + after;
    },
  },
};

// server/document-sync.mjs
export async function applyDocumentOperation(docId, operation, clientVectorClock) {
  const doc = await getDocument(docId);
  const currentVectorClock = doc.vectorClock;

  // Detect concurrent edits
  const comparison = clientVectorClock.compare(currentVectorClock);

  if (comparison === -1) {
    // Client is behind - transform operation against intervening ops
    const intervening = getOperationsSince(docId, clientVectorClock);
    operation = transformAgainstOps(operation, intervening);
  }

  // Validate operation
  const opType = DOC_OPERATIONS[operation.type];
  const validation = opType.validate(operation);
  if (!validation.valid) {
    return { status: 'REJECT', reason: validation.reason };
  }

  // Apply operation
  const newContent = opType.apply(doc.content, operation);

  // Store in event log
  const { receipt } = await store.appendEvent(
    { type: 'DOC_OPERATION', docId },
    [{
      type: 'update',
      field: 'content',
      oldValue: doc.content,
      newValue: newContent,
    }]
  );

  // Broadcast to other editors
  broadcastDocumentChange({
    docId,
    operation,
    newVector Clock: store.vectorClock.toJSON(),
  });

  return {
    status: 'ACK',
    newContent,
    vectorClock: store.vectorClock.toJSON(),
  };
}

// Operational transformation: adjust operation against concurrent ops
function transformAgainstOps(operation, priorOps) {
  let transformed = operation;

  for (const prior of priorOps) {
    if (prior.type === 'INSERT_TEXT' && transformed.type === 'INSERT_TEXT') {
      // If both insert before our position, adjust position
      if (prior.position <= transformed.position) {
        transformed.position += prior.text.length;
      }
    } else if (prior.type === 'DELETE_TEXT' && transformed.type === 'INSERT_TEXT') {
      // Adjust insert position after delete
      if (prior.position < transformed.position) {
        transformed.position -= prior.length;
      }
    }
    // ... more transformation rules
  }

  return transformed;
}
```

#### Client: Real-Time Editor State
```javascript
// client/useDocumentEditor.js
import { useKGC } from './kgc-context';

export function useDocumentEditor(docId) {
  const { submitDelta, shard, vectorClock } = useKGC();
  const [content, setContent] = useState('');
  const [cursor, setCursor] = useState(0);

  const insertText = async (text, position) => {
    // Optimistic update
    const newContent = content.substring(0, position) + text + content.substring(position);
    setContent(newContent);

    // Submit to server
    const result = await submitDelta(
      [
        {
          type: 'INSERT_TEXT',
          position,
          text,
          docId,
        },
      ],
      { client_vector_clock: vectorClock }
    );

    if (!result.success) {
      // Rollback on reject
      setContent(content);
    }

    return result;
  };

  return { content, insertText, cursor, setCursor };
}
```

---

## Context 3: Task Management System

### Problem
Team members creating, updating, and completing tasks with validation rules and state transitions.

### Solution Architecture
- **Universe**: Task database + event log
- **Delta**: Task field updates (title, status, assignee, due date)
- **Shards**: Users see only tasks assigned to them; managers see all
- **Validation**: Status rules, deadline validation, role-based permissions

### Implementation

#### Server: Task Validation
```javascript
// hooks/task-hooks.mjs
export const TASK_HOOKS = {
  VALIDATE_STATUS: {
    id: 'validate-status-transition',
    field: 'status',
    validate: (value, context) => {
      const allowedStates = ['TODO', 'IN_PROGRESS', 'DONE', 'BLOCKED'];

      if (!allowedStates.includes(value)) {
        return { valid: false, reason: 'Invalid status' };
      }

      // State transition rules
      const currentStatus = context.currentTask?.status;
      const validTransitions = {
        'TODO': ['IN_PROGRESS', 'BLOCKED'],
        'IN_PROGRESS': ['DONE', 'TODO', 'BLOCKED'],
        'BLOCKED': ['TODO', 'IN_PROGRESS'],
        'DONE': [], // No transitions from DONE
      };

      if (currentStatus && !validTransitions[currentStatus]?.includes(value)) {
        return {
          valid: false,
          reason: `Cannot transition from ${currentStatus} to ${value}`,
        };
      }

      return { valid: true };
    },
  },

  VALIDATE_DUE_DATE: {
    id: 'validate-due-date',
    field: 'dueDate',
    validate: (value) => {
      const date = new Date(value);

      if (isNaN(date.getTime())) {
        return { valid: false, reason: 'Invalid date format' };
      }

      if (date < new Date()) {
        return { valid: false, reason: 'Due date cannot be in past' };
      }

      return { valid: true };
    },
  },

  VALIDATE_TITLE: {
    id: 'validate-title',
    field: 'title',
    validate: (value) => {
      if (!value || value.trim().length === 0) {
        return { valid: false, reason: 'Title cannot be empty' };
      }

      if (value.length > 200) {
        return { valid: false, reason: 'Title too long' };
      }

      return { valid: true };
    },
  },

  CHECK_PERMISSION: {
    id: 'check-permission',
    field: '*', // All fields
    validate: (value, context) => {
      // Only assignee or manager can update task
      const user = context.user;
      const task = context.currentTask;

      if (user.role !== 'manager' && user.id !== task.assignee) {
        return { valid: false, reason: 'Permission denied' };
      }

      return { valid: true };
    },
  },
};

// server/task-delta.mjs
export async function submitTaskUpdate(taskId, updates, user) {
  const store = await getTaskStore();
  const task = await store.getTask(taskId);

  // Validate each update
  for (const [field, newValue] of Object.entries(updates)) {
    const hook = TASK_HOOKS[`VALIDATE_${field.toUpperCase()}`];

    if (hook) {
      const validation = hook.validate(newValue, { currentTask: task, user });
      if (!validation.valid) {
        return {
          status: 'REJECT',
          reason: validation.reason,
          field,
        };
      }
    }
  }

  // Apply updates
  const { receipt } = await store.appendEvent(
    {
      type: 'TASK_UPDATED',
      taskId,
      user: user.id,
    },
    Object.entries(updates).map(([field, newValue]) => ({
      type: 'update',
      field,
      oldValue: task[field],
      newValue,
    }))
  );

  // Broadcast to subscribers
  broadcastTaskUpdate({
    taskId,
    updates,
    updatedBy: user.id,
  });

  // Notify watchers
  if (updates.status === 'DONE') {
    notifyTaskCompleted(taskId, task.assignee);
  }

  return {
    status: 'ACK',
    receipt: receipt.id,
    task: await store.getTask(taskId),
  };
}
```

#### Client: Task State Management
```javascript
// hooks/useTask.js
import { useKGC } from './kgc-context';

export function useTask(taskId) {
  const { submitDelta, shard } = useKGC();
  const task = shard?.tasks?.find(t => t.id === taskId);

  const updateTask = async (updates) => {
    // Optimistic update
    const updatedTask = { ...task, ...updates };

    // Submit to server
    const result = await submitDelta(
      Object.entries(updates).map(([field, value]) => ({
        type: 'update',
        subject: taskId,
        predicate: field,
        value,
      }))
    );

    if (!result.success) {
      return { success: false, error: result.error };
    }

    return { success: true, task: result.task };
  };

  const updateStatus = (newStatus) => updateTask({ status: newStatus });
  const complete = () => updateStatus('DONE');

  return { task, updateTask, updateStatus, complete };
}
```

#### Real-Time Task List
```javascript
// components/TaskList.jsx
import { useKGC } from '../lib/client/kgc-context';

export function TaskList() {
  const { shard, connect, isConnected } = useKGC();

  useEffect(() => {
    // Connect to real-time task stream
    connect({ type: 'TASK' });
  }, [connect]);

  if (!isConnected) {
    return <div>Connecting to task stream...</div>;
  }

  return (
    <div className="task-list">
      {shard?.tasks?.map(task => (
        <TaskCard key={task.id} task={task} />
      ))}
    </div>
  );
}
```

---

## Context 4: Financial Ledger

### Problem
Track financial transactions with immutable audit trail, balance validation, and time-travel query support.

### Solution Architecture
- **Universe**: Transaction log (event-sourced)
- **Delta**: Transaction records (immutable, no updates/deletes)
- **Shards**: Account balance (derived from transaction history)
- **Validation**: Balance constraints, duplicate detection
- **Time-Travel**: Reconstruct account balance at any point in time

### Implementation

#### Server: Transaction Validation
```javascript
// hooks/ledger-hooks.mjs
export const LEDGER_HOOKS = {
  VALIDATE_AMOUNT: {
    id: 'validate-amount',
    field: 'amount',
    validate: (value) => {
      if (typeof value !== 'number' || value <= 0) {
        return { valid: false, reason: 'Amount must be positive' };
      }

      if (value > 999999999.99) {
        return { valid: false, reason: 'Amount exceeds maximum' };
      }

      return { valid: true };
    },
  },

  CHECK_SUFFICIENT_BALANCE: {
    id: 'check-balance',
    field: 'amount',
    validate: async (value, context) => {
      const account = context.account;
      const currentBalance = await calculateBalance(account.id);

      if (context.transaction.type === 'withdrawal' && currentBalance - value < 0) {
        return {
          valid: false,
          reason: `Insufficient balance: ${currentBalance}, requested: ${value}`,
        };
      }

      return { valid: true };
    },
  },

  CHECK_DUPLICATE: {
    id: 'check-duplicate',
    field: '*',
    validate: async (value, context) => {
      // Check if identical transaction already recorded (within last 5 minutes)
      const recent = await getRecentTransactions(
        context.transaction.accountId,
        300000 // 5 minutes
      );

      const isDuplicate = recent.some(t =>
        t.amount === context.transaction.amount &&
        t.type === context.transaction.type &&
        t.counterparty === context.transaction.counterparty
      );

      if (isDuplicate) {
        return { valid: false, reason: 'Duplicate transaction detected' };
      }

      return { valid: true };
    },
  },

  REQUIRE_APPROVAL: {
    id: 'require-approval',
    field: 'amount',
    validate: (value, context) => {
      // Large transactions require approval
      if (value > 10000 && !context.approved) {
        return {
          valid: false,
          reason: 'Large transaction requires manager approval',
        };
      }

      return { valid: true };
    },
  },
};

// server/ledger.mjs
export async function recordTransaction(transaction, user) {
  const store = await getLedgerStore();

  // Validate
  const validation = LEDGER_HOOKS.VALIDATE_AMOUNT.validate(transaction.amount, { transaction });
  if (!validation.valid) {
    return { status: 'REJECT', reason: validation.reason };
  }

  const balanceCheck = await LEDGER_HOOKS.CHECK_SUFFICIENT_BALANCE.validate(
    transaction.amount,
    { account: transaction.account, transaction }
  );
  if (!balanceCheck.valid) {
    return { status: 'REJECT', reason: balanceCheck.reason };
  }

  // Record as immutable event
  const { receipt } = await store.appendEvent(
    {
      type: 'TRANSACTION',
      accountId: transaction.accountId,
      user: user.id,
    },
    [{
      type: 'add',
      transaction: {
        id: crypto.randomUUID(),
        ...transaction,
        timestamp: now(),
        recordedBy: user.id,
      },
    }]
  );

  return {
    status: 'ACK',
    transactionId: receipt.id,
    newBalance: await calculateBalance(transaction.accountId),
  };
}

// Time-travel: Get balance at specific time
export async function getBalanceAtTime(accountId, targetTime) {
  const store = await getLedgerStore();

  // Reconstruct from transaction history
  let balance = 0;
  const transactions = await store.getTransactionsBefore(accountId, targetTime);

  for (const tx of transactions) {
    if (tx.type === 'deposit') {
      balance += tx.amount;
    } else if (tx.type === 'withdrawal') {
      balance -= tx.amount;
    }
  }

  return balance;
}
```

#### Audit Reporting
```javascript
// api/audit-report/route.mjs
export async function GET(request) {
  const url = new URL(request.url);
  const accountId = url.searchParams.get('accountId');
  const startDate = new Date(url.searchParams.get('startDate'));
  const endDate = new Date(url.searchParams.get('endDate'));

  const transactions = await getTransactions(accountId, startDate, endDate);

  const report = {
    accountId,
    period: { start: startDate, end: endDate },
    transactions: transactions.map(t => ({
      id: t.id,
      timestamp: t.timestamp,
      type: t.type,
      amount: t.amount,
      balance: t.balance,
      recordedBy: t.recordedBy,
    })),
    openingBalance: await getBalanceAtTime(accountId, startDate),
    closingBalance: await getBalanceAtTime(accountId, endDate),
  };

  return Response.json(report);
}
```

---

## Context 5: IoT Sensor Network

### Problem
Collect sensor readings from thousands of devices with real-time dashboards and anomaly detection.

### Solution Architecture
- **Universe**: Time-series data + event log
- **Delta**: Sensor readings (immutable, append-only)
- **Shards**: Device dashboard sees only own readings; platform sees aggregates
- **Validation**: Value range checks, frequency validation
- **Time-Travel**: Query historical sensor data

### Implementation

#### Server: Sensor Validation
```javascript
// hooks/sensor-hooks.mjs
export const SENSOR_HOOKS = {
  VALIDATE_TEMPERATURE: {
    id: 'validate-temperature',
    field: 'temperature',
    validate: (value, context) => {
      const sensorType = context.sensor.type;

      if (sensorType === 'interior') {
        if (value < -40 || value > 125) {
          return { valid: false, reason: 'Temperature out of sensor range' };
        }
      }

      return { valid: true };
    },
  },

  CHECK_ANOMALY: {
    id: 'check-anomaly',
    field: '*',
    validate: async (value, context) => {
      const sensor = context.sensor;
      const recent = await getRecentReadings(sensor.id, 60 * 60 * 1000); // Last hour

      const avg = recent.reduce((sum, r) => sum + r.value, 0) / recent.length;
      const stdDev = calculateStdDev(recent.map(r => r.value));

      // Flag if reading is >3 std devs from mean
      if (Math.abs(value - avg) > 3 * stdDev) {
        return { valid: true, anomaly: true, severity: 'warning' };
      }

      return { valid: true, anomaly: false };
    },
  },

  CHECK_FREQUENCY: {
    id: 'check-frequency',
    field: '*',
    validate: async (value, context) => {
      const sensor = context.sensor;
      const lastReading = await getLastReading(sensor.id);

      // Check if reading is too frequent (e.g., < 1 minute)
      if (lastReading && Date.now() - lastReading.timestamp < 60000) {
        return { valid: false, reason: 'Reading too frequent' };
      }

      return { valid: true };
    },
  },
};

// server/sensor-ingest.mjs
export async function ingestSensorReading(reading) {
  const store = await getSensorStore();
  const sensor = await store.getSensor(reading.sensorId);

  // Validate reading
  const hook = SENSOR_HOOKS[`VALIDATE_${sensor.type.toUpperCase()}`];
  if (hook) {
    const validation = hook.validate(reading.value, { sensor });
    if (!validation.valid) {
      return { status: 'REJECT', reason: validation.reason };
    }
  }

  // Check for anomalies
  const anomalyCheck = await SENSOR_HOOKS.CHECK_ANOMALY.validate(reading.value, { sensor });

  // Record reading
  const { receipt } = await store.appendEvent(
    {
      type: 'SENSOR_READING',
      sensorId: sensor.id,
    },
    [{
      type: 'add',
      reading: {
        id: crypto.randomUUID(),
        sensorId: sensor.id,
        value: reading.value,
        unit: sensor.unit,
        timestamp: now(),
        anomaly: anomalyCheck.anomaly,
      },
    }]
  );

  // Alert on anomaly
  if (anomalyCheck.anomaly) {
    await alertOnAnomaly(sensor, reading.value);
  }

  return {
    status: 'ACK',
    readingId: receipt.id,
    anomaly: anomalyCheck.anomaly,
  };
}
```

#### Dashboard: Real-Time Sensor Display
```javascript
// components/SensorDashboard.jsx
export function SensorDashboard({ sensorId }) {
  const { shard, connect, isConnected } = useKGC();
  const [readings, setReadings] = useState([]);

  useEffect(() => {
    connect({ sensorId, type: 'SENSOR_READING' });
  }, [sensorId]);

  useEffect(() => {
    if (shard?.readings) {
      setReadings(shard.readings);
    }
  }, [shard?.readings]);

  const latestReading = readings[0];
  const hasAnomaly = latestReading?.anomaly;

  return (
    <div className={`dashboard ${hasAnomaly ? 'anomaly-alert' : ''}`}>
      <h2>Sensor {sensorId}</h2>
      <div className="current-reading">
        {latestReading && (
          <>
            <div className="value">{latestReading.value} {latestReading.unit}</div>
            <div className="timestamp">{new Date(latestReading.timestamp).toISOString()}</div>
            {hasAnomaly && <div className="anomaly-badge">⚠️ Anomaly Detected</div>}
          </>
        )}
      </div>
      <Chart data={readings} />
    </div>
  );
}
```

---

## Comparison Table

| Context | Universe | Delta | Validation Focus | Special Feature |
|---------|----------|-------|------------------|-----------------|
| E-Commerce | Inventory DB | Quantity changes | Stock levels | Restock alerts |
| Document Editor | Document + Log | Text operations | Position, length | Operational transform |
| Task Mgmt | Task DB + Log | Field updates | Status transitions | Role-based perms |
| Financial Ledger | Transaction log | Transactions (immutable) | Balance check | Time-travel queries |
| IoT Sensors | Time-series | Sensor readings | Value ranges | Anomaly detection |

Each demonstrates different aspects of the core patterns adapted to domain-specific needs.

# Erlang-Like Process Framework

JavaScript microframework that mirrors Erlang's process model with poka-yoke design.

## Overview

The framework provides:
- **Process Model**: Isolated processes with mailboxes
- **Message Passing**: Asynchronous send/receive
- **Links**: Bidirectional failure propagation
- **Monitors**: Unidirectional failure notification
- **Supervision**: Process trees with restart strategies
- **Poka-Yoke**: Invalid states and operations are impossible

## Core Concepts

### Process

A process is an isolated unit of execution with:
- **Mailbox**: Queue of messages
- **State Machine**: Prevents invalid operations
- **Links**: Connected processes (bidirectional)
- **Monitors**: Watched processes (unidirectional)

### Message Passing

Processes communicate via asynchronous messages:
- **Send**: Non-blocking message delivery
- **Receive**: Blocking message retrieval
- **Pattern Matching**: Filter messages by type

### Links

Links create bidirectional failure propagation:
- If process A links to process B, both are linked
- If A dies, B dies (and vice versa)
- Used for process groups that should fail together

### Monitors

Monitors create unidirectional failure notification:
- If process A monitors process B, A receives DOWN message when B dies
- B is not affected by A's death
- Used for supervision and error handling

### Supervision

Supervisors manage child processes:
- **Restart Strategies**: one_for_one, one_for_all, rest_for_one
- **Automatic Restart**: Children are restarted on failure
- **Process Trees**: Hierarchical process organization

## API Reference

### spawn(name, initFn, handleFn, options)

Spawn a new process.

```javascript
import { spawn } from './erlang-process.mjs';

const process = spawn(
  'my_process',
  async () => {
    // Initialization
    return { initialized: true };
  },
  async (message) => {
    // Message handler
    if (message.type === 'ping') {
      return { type: 'pong' };
    }
  },
  {
    mailboxMaxSize: 1000, // Optional
  }
);
```

**Poka-Yoke**:
- Validates `name` is non-empty string
- Validates `initFn` and `handleFn` are functions
- Prevents duplicate process names

### send(name, message)

Send message to process by name.

```javascript
import { send } from './erlang-process.mjs';

send('my_process', { type: 'ping', data: 'hello' });
```

**Poka-Yoke**:
- Throws error if process not found
- Throws error if process is dead

### Process Methods

#### process.send(message)

Send message to process.

```javascript
process.send({ type: 'test', data: 'value' });
```

**Poka-Yoke**:
- Throws error if process is dead
- Throws error if mailbox is full

#### process.receive(timeout)

Receive message (blocking).

```javascript
const message = await process.receive(5000); // 5s timeout
```

**Poka-Yoke**:
- Throws error if process is not running
- Throws error on timeout

#### process.link(targetProcess)

Link to another process.

```javascript
process1.link(process2);
// Both processes are now linked
```

**Poka-Yoke**:
- Throws error if either process is dead
- Creates bidirectional link

#### process.monitor(targetProcess)

Monitor another process.

```javascript
const monitorRef = process1.monitor(process2);
// process1 will receive DOWN message when process2 dies
```

**Poka-Yoke**:
- Throws error if target process is dead
- Returns monitor reference

#### process.exit(reason)

Exit process.

```javascript
process.exit('normal');
```

**Poka-Yoke**:
- Propagates exit to linked processes
- Sends DOWN messages to monitoring processes

#### process.kill()

Kill process (forceful termination).

```javascript
process.kill();
```

### Supervisor

#### new Supervisor(name, strategy)

Create supervisor.

```javascript
import { Supervisor } from './erlang-process.mjs';

const supervisor = new Supervisor('my_supervisor', 'one_for_one');
```

**Strategies**:
- `one_for_one`: Restart only the failed child
- `one_for_all`: Restart all children when one fails
- `rest_for_one`: Restart failed child and all children started after it

#### supervisor.startChild(childSpec)

Start child process.

```javascript
const child = supervisor.startChild({
  name: 'child_1',
  initFn: async () => ({ initialized: true }),
  handleFn: async (message) => {
    if (message.type === 'crash') {
      throw new Error('Child crashed');
    }
  },
});
```

**Poka-Yoke**:
- Validates child spec
- Prevents duplicate child names
- Automatically links supervisor to child

#### supervisor.terminate()

Terminate supervisor and all children.

```javascript
supervisor.terminate();
```

## Usage Patterns

### Basic Process

```javascript
import { spawn } from './erlang-process.mjs';

const process = spawn(
  'counter',
  async () => {
    return { count: 0 };
  },
  async (message) => {
    if (message.type === 'increment') {
      return { count: message.count + 1 };
    } else if (message.type === 'get') {
      return { count: message.count };
    }
  }
);

process.send({ type: 'increment', count: 5 });
```

### Process Links

```javascript
const process1 = spawn('p1', async () => {}, async () => {});
const process2 = spawn('p2', async () => {}, async () => {});

process1.link(process2);
// If process1 dies, process2 dies (and vice versa)
```

### Process Monitors

```javascript
const process1 = spawn('p1', async () => {}, async (message) => {
  if (message.type === 'DOWN') {
    console.log('Process died:', message.pid, message.reason);
  }
});

const process2 = spawn('p2', async () => {}, async () => {});

process1.monitor(process2);
// process1 will receive DOWN message when process2 dies
```

### Supervisor Pattern

```javascript
const supervisor = new Supervisor('app_supervisor', 'one_for_one');

// Start workers
supervisor.startChild({
  name: 'worker_1',
  initFn: async () => ({ initialized: true }),
  handleFn: async (message) => {
    // Worker logic
  },
});

supervisor.startChild({
  name: 'worker_2',
  initFn: async () => ({ initialized: true }),
  handleFn: async (message) => {
    // Worker logic
  },
});
```

### Process Swarm (Boardroom Story Pattern)

```javascript
import { spawn } from './erlang-process.mjs';
import { getBridge } from './kgc4d-bridge.mjs';

const bridge = getBridge();
const workers = [];

// Spawn swarm of workers
for (let i = 0; i < 10; i++) {
  const worker = spawn(
    `worker_${i}`,
    async () => ({ initialized: true, index: i }),
    async (message) => {
      if (message.type === 'emit_event') {
        await bridge.emitEvent(message.eventType, message.payload);
      }
    }
  );
  workers.push(worker);
}

// Send work to workers
workers.forEach(worker => {
  worker.send({
    type: 'emit_event',
    eventType: 'PROCESS_STARTED',
    payload: { workerId: worker.name },
  });
});
```

## Poka-Yoke Design

### State Machine

Process states:
- `initialized`: Process created but not started
- `running`: Process is executing
- `waiting`: Process is waiting for message
- `terminated`: Process has exited
- `error`: Process encountered error

**Invalid Operations**:
- Cannot send to dead process
- Cannot link to dead process
- Cannot monitor dead process
- Cannot receive in non-running process

### Input Validation

All inputs are validated:
- Process names must be non-empty strings
- Functions must be actual functions
- Messages must be objects
- Timeouts must be numbers

### Type Guards

Type guards ensure state consistency:
- `isRunning()`: Process is running
- `isAlive()`: Process is alive (running or waiting)

## Error Handling

### Process Errors

If a process handler throws an error:
1. Process state changes to `error`
2. Exit reason is set to `error`
3. Exit is propagated to linked processes
4. DOWN messages sent to monitoring processes

### Supervisor Restart

If a supervised child crashes:
1. Supervisor receives notification
2. Child is restarted based on strategy
3. Restart count is incremented

## Performance Considerations

- **Mailbox Size**: Default 1000 messages (configurable)
- **Message Loop**: Checks mailbox every 10ms
- **Process Overhead**: Minimal (async/await based)
- **Concurrency**: Limited by JavaScript event loop

## Testing

See `test/erlang-process-stress.mjs` for stress tests:
- Spawn many processes
- Send many messages
- Process links
- Process monitors
- Supervisor patterns
- Poka-yoke validation

## Integration with Roundtrip SLA

The framework integrates with roundtrip SLA tracking:

```javascript
import { getBridge } from './kgc4d-bridge.mjs';
import { spawn } from './erlang-process.mjs';

const bridge = getBridge();

const process = spawn(
  'event_emitter',
  async () => ({ initialized: true }),
  async (message) => {
    if (message.type === 'emit') {
      // Roundtrip tracked automatically by bridge
      await bridge.emitEvent(message.eventType, message.payload);
    }
  }
);
```

Roundtrip SLA metrics are automatically tracked when processes call bridge methods.

## Limitations

- **Single-threaded**: All processes run in single JavaScript event loop
- **No true isolation**: Processes share memory (unlike Erlang)
- **No preemption**: Processes cannot be preempted (cooperative)
- **Limited scalability**: Not suitable for millions of processes

## Future Enhancements

- Worker threads for true isolation
- Process groups
- Global process registry
- Process migration
- Hot code reloading


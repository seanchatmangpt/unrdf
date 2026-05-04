# Service Worker Strategy Explained

## Why Service Workers?

Service workers enable Cross-Origin-Isolation without server configuration, making them ideal for static hosting and client-side applications.

## How coi-serviceworker Works

The `coi-serviceworker` library uses a service worker to intercept all fetch requests and add the required COOP/COEP headers, enabling COI automatically.

## Request Interception Flow

1. Service worker registers on page load
2. Intercepts all fetch requests
3. Adds headers to responses
4. Browser recognizes COI context

## Activation Requirements

- Service worker must be activated
- Page reload needed for first activation
- HTTPS or localhost required

## Related Documentation

- [Cross-Origin-Isolation Explanation](./cross-origin-isolation.md)
- [Architecture Overview](./architecture.md)
- [How-To: Debug Service Worker Issues](../how-to/debug-service-worker.md)



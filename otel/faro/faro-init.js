/* global window, document */
/**
 * Grafana Faro Web SDK — Real User Monitoring
 *
 * Drop this <script> tag into any web UI to capture:
 *   - Page loads, navigation, interactions
 *   - JavaScript errors (unhandled + unrejected promises)
 *   - Long tasks, layout shifts, paint metrics
 *   - Custom events from your application
 *
 * Configuration:
 *   FARO_ENDPOINT — OTEL Collector OTLP HTTP endpoint (default: localhost:4318)
 *   APP_NAME      — Application identifier (default: unrdf-web)
 *   APP_VERSION   — Application version (default: 1.0.0)
 *
 * Usage:
 *   <script src="path/to/faro-init.js"></script>
 *   <!-- Or inline the content -->
 *
 *   <script>
 *     window.__FARO?.setUser({ id: 'user-123', email: 'user@example.com' });
 *     window.__FARO?.pushEvent('custom_event', { key: 'value' });
 *   </script>
 */

(function() {
  'use strict';

  const config = {
    endpoint: window.__FARO_CONFIG?.endpoint || 'http://localhost:4318/v1/logs',
    appName: window.__FARO_CONFIG?.appName || 'unrdf-web',
    appVersion: window.__FARO_CONFIG?.appVersion || '1.0.0',
    environment: window.__FARO_CONFIG?.environment || 'development',
  };

  // Faro Web SDK CDN URL (loads the full bundle)
  // For production, vendor this or use npm: @grafana/faro-web-sdk
  const FARO_SDK_URL = 'https://unpkg.com/@grafana/faro-web-sdk@1.9.0/dist/faro-web-sdk.iife.js';
  const FARO_WEB_VITALS_URL = 'https://unpkg.com/@grafana/faro-web-vitals@1.9.0/dist/faro-web-vitals.iife.js';

  function initFaro() {
    if (!window.GrafanaFaroWebSdk || !window.GrafanaFaroWebVitals) {
      setTimeout(initFaro, 100);
      return;
    }

    const { initializeFaro, FaroSessionManager } = window.GrafanaFaroWebSdk;

    const faro = initializeFaro({
      url: config.endpoint.replace('/v1/logs', '/v1/traces'), // Faro sends to traces endpoint
      appId: config.appName,
      appVersion: config.appVersion,
      sessionManager: new FaroSessionManager({ sessionSampleRate: 1.0 }),
      instrumentations: [
        // Web Vitals: LCP, FID, CLS, TTFB, INP
        window.GrafanaFaroWebVitals.instrumentation(),
      ],
      api: {
        beforeSend: (payload) => {
          // Add custom attributes
          if (payload && payload.meta) {
            payload.meta.appName = config.appName;
            payload.meta.environment = config.environment;
          }
          return payload;
        },
      },
    });

    // Expose faro instance for custom usage
    window.__FARO = {
      pushEvent: (name, attributes) => faro.pushEvent(name, attributes),
      pushMeasurement: (name, value) => faro.pushMeasurement({ type: name, value }),
      pushLog: (level, message, context) => faro.api.pushLog([{ level, message, context, timestamp: Date.now() }]),
      setUser: (user) => faro.api.setUser({ ...user }),
      getTraceId: () => {
        try {
          return faro.api.getTraceContext()?.traceId;
        } catch { return null; }
      },
    };

    console.log(`[Faro] Initialized — app: ${config.appName}, version: ${config.appVersion}`);
  }

  // Load SDKs dynamically
  function loadScript(url, callback) {
    const script = document.createElement('script');
    script.src = url;
    script.async = true;
    script.onload = callback;
    script.onerror = () => console.warn(`[Faro] Failed to load ${url}`);
    document.head.appendChild(script);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', () => {
      loadScript(FARO_SDK_URL, () => loadScript(FARO_WEB_VITALS_URL, initFaro));
    });
  } else {
    loadScript(FARO_SDK_URL, () => loadScript(FARO_WEB_VITALS_URL, initFaro));
  }
})();

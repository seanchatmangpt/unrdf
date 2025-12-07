# Page snapshot

```yaml
- generic [ref=e2]:
  - heading "⚛️ AtomVM Browser Runtime" [level=1] [ref=e3]
  - paragraph [ref=e4]: Erlang/BEAM VM running in your browser via WebAssembly
  - generic [ref=e5]: "❌ Error: Service worker registration failed"
  - generic [ref=e6]:
    - generic [ref=e7]: AtomVM Browser Console
    - generic [ref=e8]: ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    - generic [ref=e9]: Starting AtomVM browser initialization...
    - generic [ref=e10]: Registering coi-serviceworker...
    - generic [ref=e11]: "[8:45:31 PM] Initialization failed: Service worker registration failed"
  - generic [ref=e12]:
    - button "Initialize AtomVM" [disabled] [ref=e13]
    - button "Run Example" [disabled] [ref=e14]
    - button "Clear Console" [ref=e15] [cursor=pointer]
  - generic [ref=e16]:
    - heading "🔧 Technical Details" [level=3] [ref=e17]
    - list [ref=e18]:
      - listitem [ref=e19]:
        - code [ref=e20]: coi-serviceworker
        - text: enables SharedArrayBuffer support
      - listitem [ref=e21]: Cross-Origin-Isolation via COOP/COEP headers
      - listitem [ref=e22]: AtomVM WASM running with threading support
      - listitem [ref=e23]: "Service Worker status: pending"
```
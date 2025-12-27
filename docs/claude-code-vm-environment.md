# Claude Code Web VM Environment

## Executive Summary

Claude Code on the web runs inside a **gVisor (runsc) container** on **Google Cloud Platform (GCE)**, NOT a traditional VM or microVM. All egress traffic is routed through Anthropic's JWT-authenticated security proxy. This document provides empirical fingerprinting evidence and architectural implications for development.

---

## Environment Fingerprinting Results

### Sandbox Technology: gVisor

| Check | Result | Interpretation |
|-------|--------|----------------|
| `systemd-detect-virt -v` | `google` | Running on Google Cloud |
| `uname -r` | `4.4.0` | gVisor's emulated kernel version |
| `uname -n` | `runsc` | **gVisor's container runtime** |
| `/.dockerenv` | Present | Docker-compatible container |
| `/dev/kvm` | Not present | No nested virtualization |
| `lsmod` | Not available | User-space kernel (no modules) |

**Conclusion**: This is a **gVisor sandboxed container**, not a VM. gVisor provides:
- User-space kernel emulation (stronger isolation than traditional containers)
- OCI/Docker compatibility
- Syscall interception and filtering
- The 4.4.0 kernel version is the *emulated* Linux kernel gVisor presents

### Infrastructure: Google Cloud Platform

```
DMI Product Name: Google Compute Engine
Virtualization:   google (GCE)
```

### Container Identity

```
Cgroup paths:
  container_01MokkCUj6dciuaj9PyAUpJ4--claude_code_remote--kindly-half-rash-trips

Container naming convention:
  container_{UUID}--claude_code_remote--{human-readable-slug}
```

---

## Resource Allocation

| Resource | Value | Notes |
|----------|-------|-------|
| **vCPU** | 16 cores | Intel Ice Lake (model 106) @ 2.6 GHz |
| **RAM** | ~21 GB | `MemTotal: 22020096 kB` |
| **Root FS** | 30 GB | 5% used at session start |
| **Dev volumes** | 252 GB | `/dev`, `/dev/shm`, `/sys/fs/cgroup` |

### CPU Capabilities

AVX-512 full support: `avx512f avx512dq avx512cd avx512bw avx512vl avx512vbmi avx512_vbmi2 avx512_vnni avx512_bitalg avx512_vpopcntdq`

Useful for WASM SIMD optimizations (e.g., PDF rendering).

---

## Operating System & Toolchain

| Component | Version |
|-----------|---------|
| **OS** | Ubuntu 24.04.3 LTS (Noble Numbat) |
| **Node.js** | v22.21.1 |
| **npm** | 10.9.4 |
| **pnpm** | 10.25.0 |
| **Git** | Available at `/usr/bin/git` |
| **curl/wget** | Available |

---

## Network & Egress Architecture

### Proxy Configuration

All HTTP/HTTPS traffic is routed through Anthropic's egress control proxy:

```
Proxy endpoint: 21.0.0.191:15004
Authentication: JWT (ES256, signed by anthropic-egress-control)
```

### JWT Token Claims (Decoded)

```json
{
  "iss": "anthropic-egress-control",
  "organization_uuid": "{org-uuid}",
  "session_id": "session_{id}",
  "container_id": "container_{uuid}--claude_code_remote--{slug}",
  "allowed_hosts": "*",
  "is_hipaa_regulated": "false",
  "use_egress_gateway": "false",
  "iat": {timestamp},
  "exp": {timestamp+4h}
}
```

### Proxy Bypass List (no_proxy)

```
localhost, 127.0.0.1, 169.254.169.254,
metadata.google.internal, *.svc.cluster.local,
*.local, *.googleapis.com, *.google.com
```

### Environment Variables Set

| Variable | Purpose |
|----------|---------|
| `HTTP_PROXY` / `HTTPS_PROXY` | Standard proxy vars |
| `GLOBAL_AGENT_*_PROXY` | Node.js global-agent |
| `YARN_HTTP_PROXY` | Yarn package manager |
| `CLAUDE_CODE_PROXY_RESOLVES_HOSTS` | Proxy handles DNS |
| `CCR_TEST_GITPROXY` | Git operations proxied |
| `ELECTRON_GET_USE_PROXY` | Electron downloads |

---

## Security Model

### Trust Boundaries

```
┌─────────────────────────────────────────────────────────────┐
│  Claude Code Session (gVisor sandbox)                       │
│  ┌─────────────────────────────────────────────────────┐   │
│  │  User code execution (Node.js, bash, etc.)          │   │
│  │  - No raw network access                            │   │
│  │  - No host filesystem access                        │   │
│  │  - No kernel modules                                │   │
│  │  - Syscalls intercepted by gVisor                   │   │
│  └─────────────────────────────────────────────────────┘   │
│                           │                                  │
│                           ▼                                  │
│  ┌─────────────────────────────────────────────────────┐   │
│  │  Egress Proxy (JWT-authenticated)                   │   │
│  │  - Validates allowed_hosts                          │   │
│  │  - Session-scoped credentials                       │   │
│  │  - HIPAA compliance flags                           │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│  Git Proxy (separate credential boundary)                   │
│  - Validates branch/destination                             │
│  - Attaches scoped tokens                                   │
│  - Prevents credential exfiltration                         │
└─────────────────────────────────────────────────────────────┘
```

### What This Means for Development

1. **No direct network access**: All traffic goes through proxy
2. **Git credentials are proxied**: Never visible to user code
3. **Session isolation**: Each session gets fresh container
4. **Syscall filtering**: gVisor blocks certain system calls
5. **No kernel modification**: Can't load modules, no `/dev/kvm`

---

## Implications for Pure JS LaTeX→PDF

### Constraints

| Constraint | Reason | Mitigation |
|------------|--------|------------|
| No native TeX installation | Minimal container image | Use WASM-compiled TeX |
| Egress restrictions possible | Organization may restrict | Vendor all dependencies |
| No persistent storage | Session-scoped container | Generate artifacts per-session |
| Network latency | Proxy overhead | Pre-bundle CTAN packages |

### Recommended Architecture

```
┌─────────────────────────────────────────────────────────┐
│  Pure JS LaTeX Pipeline                                 │
│                                                         │
│  1. WASM TeX Engine (vendored)                          │
│     - swiftlatex/pdflatex-wasm or similar              │
│     - Pre-compiled WASM binary in repo                  │
│                                                         │
│  2. Virtual Filesystem (memfs)                          │
│     - Mount pre-bundled CTAN packages                   │
│     - User .tex files as overlay                        │
│                                                         │
│  3. Node.js Orchestration                               │
│     - No shell-out to external TeX                      │
│     - Pure JS/WASM execution                            │
│     - AVX-512 SIMD for WASM (available!)               │
│                                                         │
│  4. Output                                              │
│     - PDF as Buffer/Uint8Array                          │
│     - Write to session filesystem or return             │
└─────────────────────────────────────────────────────────┘
```

### Verified Capabilities

- **WASM execution**: Full support (Node.js 22)
- **SIMD instructions**: AVX-512 available for WASM optimization
- **Filesystem**: 30GB available for working files
- **Memory**: 21GB for large document compilation
- **CPU**: 16 cores for parallel chapter compilation

---

## Fingerprinting Commands Reference

Run these to identify your Claude Code environment:

```bash
# Sandbox type
systemd-detect-virt -v          # → google (GCE)
uname -a                        # → Linux runsc 4.4.0 (gVisor)

# Container markers
cat /proc/1/cgroup              # → container paths
test -f /.dockerenv && echo yes # → yes (Docker-style)

# Hardware info
cat /sys/class/dmi/id/product_name  # → Google Compute Engine

# Proxy/network model
env | grep -i proxy             # → Anthropic egress control

# Resources
nproc                           # → 16
free -h                         # → ~21G
df -h                           # → 30G root, 252G dev
```

---

## Comparison: Claude Code Environments

| Aspect | Web (This Doc) | Local (macOS/Linux) |
|--------|----------------|---------------------|
| Runtime | gVisor container | Native + bubblewrap/Seatbelt |
| Isolation | Container + proxy | OS sandbox primitives |
| Network | Egress proxy (mandatory) | Proxy (optional) |
| Persistence | Session-scoped | User filesystem |
| Git credentials | Proxy-mediated | Local keychain |
| Performance | Cloud resources (16 CPU) | Local hardware |

---

## References

- [Anthropic: Claude Code Security](https://docs.anthropic.com/en/docs/claude-code/security) - Official security documentation
- [gVisor Project](https://gvisor.dev/) - Container sandbox runtime
- [GCE Instance Identity](https://cloud.google.com/compute/docs/instances/verifying-instance-identity) - GCP verification

---

*Document generated: 2025-12-27*
*Environment: Claude Code Web (gVisor on GCE)*

# Installation Guide

This guide covers detailed installation instructions for Knowd across various platforms and deployment scenarios.

## System Requirements

### Minimum Requirements
- **Operating System:** Linux, macOS, or Windows
- **CPU:** 2 cores (Intel x64 or ARM64)
- **Memory:** 4GB RAM
- **Storage:** 10GB available space
- **Network:** Internet connection for dependencies

### Recommended Requirements
- **CPU:** 8+ cores (Intel x64 preferred)
- **Memory:** 32GB+ RAM
- **Storage:** 1TB+ NVMe SSD
- **Network:** 10Gbps connection

## Prerequisites

### Required Software

#### Bazel (Required for building)
```bash
# Install Bazel 7.0+
# macOS with Homebrew
brew install bazel

# Ubuntu/Debian
curl -fsSL https://bazel.build/bazel-release.pub.gpg | gpg --dearmor > bazel.gpg
sudo mv bazel.gpg /etc/apt/trusted.gpg.d/
echo "deb [arch=amd64] https://storage.googleapis.com/bazel-apt stable jdk1.8" | sudo tee /etc/apt/sources.list.d/bazel.list
sudo apt update && sudo apt install bazel

# Windows
# Download installer from https://github.com/bazelbuild/bazel/releases
```

#### Go (Downloaded automatically by Bazel)
Bazel will automatically download Go 1.22+ via `rules_go`. Manual installation is not required.

#### Git (Required for source)
```bash
# macOS
brew install git

# Ubuntu/Debian
sudo apt install git

# Windows
# Download from https://git-scm.com/downloads
```

### Development Tools (Optional)

```bash
# Go development tools
go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest
go install golang.org/x/tools/cmd/goimports@latest

# Protocol buffer compilation
go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest
```

## Installation Methods

### Method 1: Source Build (Recommended)

#### 1. Clone Repository
```bash
# Clone the repository
git clone https://github.com/unrdf/knowd.git
cd knowd

# Check out latest release
git checkout v1.0.0
```

#### 2. Build Binary
```bash
# Build for current platform
bazel build //cmd/knowd:knowd

# Build with debug symbols
bazel build //cmd/knowd:knowd --compilation_mode=dbg

# Build statically linked binary
bazel build //cmd/knowd:knowd --define=linkmode=fully-static
```

#### 3. Run Binary
```bash
# Run directly from bazel-bin
./bazel-bin/cmd/knowd/knowd_/knowd

# Or run via Bazel
bazel run //cmd/knowd:knowd

# With custom configuration
./bazel-bin/cmd/knowd/knowd_/knowd -addr :9090 -data-dir /var/lib/knowd
```

### Method 2: Pre-built Releases

#### Download Release
```bash
# Check latest releases at https://github.com/unrdf/knowd/releases
curl -L https://github.com/unrdf/knowd/releases/download/v1.0.0/knowd-linux-amd64 -o knowd
chmod +x knowd

# macOS
curl -L https://github.com/unrdf/knowd/releases/download/v1.0.0/knowd-darwin-amd64 -o knowd
chmod +x knowd
```

#### Verify Binary
```bash
# Verify installation
./knowd -version

# Expected output
# Version: v1.0.0
# Commit: abc123def456...
# Build: linux-amd64
```

### Method 3: Docker Deployment

#### Build Docker Image
```bash
# Clone repository
git clone https://github.com/unrdf/knowd.git
cd knowd

# Build Docker image
docker build -t knowd:latest .

# Or pull pre-built image
docker pull unrdf/knowd:v1.0.0
```

#### Run Docker Container
```bash
# Run with default configuration
docker run -p 8090:8090 knowd:latest

# Run with custom settings
docker run -p 8090:8090 \
  -v /data/knowd:/var/lib/knowd \
  -e KNOWD_DATA_DIR=/var/lib/knowd \
  knowd:latest
```

### Method 4: Kubernetes Deployment

#### Create Namespace
```bash
kubectl create namespace knowd
```

#### Deploy with Helm
```bash
# Add Helm repository
helm repo add knowd https://charts.unrdf.dev
helm repo update

# Install Knowd
helm install knowd knowd/knowd \
  --namespace knowd \
  --set data.persistence.enabled=true \
  --set data.persistence.size=100Gi
```

#### Custom Configuration
```bash
# Create custom values file
cat > values.yaml << EOF
data:
  persistence:
    enabled: true
    size: 500Gi
    storageClass. "fast-ssd"

service:
  type: LoadBalancer
  port: 8090

resources:
  requests:
    cpu: 4
    memory: 16Gi
  limits:
    cpu: 8
    memory: 32Gi
EOF

# Deploy with custom configuration
helm install ./helm-chart \
  --namespace knowd \
  --values values.yaml
```

## Platform-Specific Installation

### Ubuntu/Debian

#### Prerequisites
```bash
# Update package list
sudo apt update

# Install dependencies
sudo apt install -y curl wget gnupg lsb-release

# Install Bazel
curl -fsSL https://bazel.build/bazel-release.pub.gpg | gpg --dearmor > bazel.gpg
sudo mv bazel.gpg /etc/apt/trusted.gpg.d/
echo "deb [arch=amd64] https://storage.googleapis.com/bazel-apt stable jdk1.8" | sudo tee /etc/apt/sources.list.d/bazel.list
sudo apt update
sudo apt install bazel
```

#### ARM64 Support
```bash
# For ARM64 platforms
echo "deb [arch=arm64] https://storage.googleapis.com/bazel-apt stable jdk1.8" | sudo tee /etc/apt/sources.list.d/bazel.list
sudo apt update
sudo apt install bazel
```

### CentOS/RHEL/Fedora

```bash
# Install Bazel
curl -LSs https://github.com/bazelbuild/bazel/releases/download/7.0.0/bazel-7.0.0-installer-linux-x86_64.sh -O
sudo bash bazel-7.0.0-installer-linux-x86_64.sh

# Install dependencies
sudo dnf install -y git curl

# Clone and build
git clone https://github.com/unrdf/knowd.git
cd knowd
bazel build //cmd/knowd:knowd
```

### macOS

#### Using Homebrew
```bash
# Install Homebrew (if not already installed)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install Bazel
brew install bazel

# Install Git
brew install git

# Build Knowd
git clone https://github.com/unrdf/knowd.git
cd knowd
bazel build //cmd/knowd:knowd
```

#### Manual Installation
```bash
# Download Bazel binary
curl -LO https://github.com/bazelbuild/bazel/releases/download/7.0.0/bazel-7.0.0-darwin-x86_64
chmod +x bazel-7.0.0-darwin-x86_64
sudo mv bazel-7.0.0-darwin-x86_64 /usr/local/bin/bazel

# Build Knowd
git clone https://github.com/unrdf/knowd.git
cd knowd
bazel build //cmd/knowd:knowd
```

### Windows

#### Using WSL2 (Recommended)
```bash
# Enable WSL2 and Ubuntu
wsl --install

# Install Bazel in WSL2
curl -fsSL https://bazel.build/bazel-release.pub.gpg | gpg --dearmor > bazel.gpg
sudo mv bazel.gpg /etc/apt/trusted.gpg.d/
echo "deb [arch=amd64] https://storage.googleapis.com/bazel-apt stable jdk1.8" | sudo tee /etc/apt/sources.list.d/bazel.list
sudo apt update
sudo apt install bazel

# Build in WSL2
git clone https://github.com/unrdf/knowd.git
cd knowd
bazel build //cmd/knowd:knowd
```

#### Native Windows
```powershell
# Install Chocolatey (if not already installed)
Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))

# Install dependencies
choco install git bazel

# Build Knowd
git clone https://github.com/unrdf/knowd.git
cd knowd
bazel build //cmd/knowd:knowd
```

## Configuration Installation

### Data Directory Setup

```bash
# Create data directory
sudo mkdir -p /var/lib/knowd
sudo chown $(whoami):$(whoami) /var/lib/knowd

# Create logs directory
sudo mkdir -p /var/log/knowd
sudo chown $(whoami):$(whoami) /var/log/knowd

# Create config directory
sudo mkdir -p /etc/knowd
sudo chown $(whoami):$(whoami) /etc/knowd
```

### System Service (systemd)

#### Create Service File
```bash
sudo tee /etc/systemd/system/knowd.service << EOF
[Unit]
Description=Knowd Knowledge Graph Database
After=network.target

[Service]
Type=simple
User=knowd
Group=knowd
ExecStart=/usr/local/bin/knowd
WorkingDirectory=/var/lib/knowd
Environment=KNOWD_DATA_DIR=/var/lib/knowd
Environment=KNOWD_LOG_LEVEL=info
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
EOF
```

#### Enable Service
```bash
# Create user
sudo useradd -r -s /bin/false knowd

# Copy binary
sudo cp ./bazel-bin/cmd/knowd/knowd_/knowd /usr/local/bin/knowd
sudo chown knowd:knowd /usr/local/bin/knowd

# Set up permissions
sudo chown -R knowd:knowd /var/lib/knowd
sudo chown -R knowd:knowd /var/log/knowd

# Enable and start service
sudo systemctl daemon-reload
sudo systemctl enable knowd
sudo systemctl start knowd

# Check status
sudo systemctl status knowd
```

## Verification

### Functional Testing

```bash
# Test health endpoint
curl http://localhost:8090/healthz

# Test version endpoint
curl http://localhost:8090/version

# Test basic transaction
curl -X POST http://localhost:8090/v1/tx \
  -H "Content-Type: application/json" \
  -d '{"delta": {"add": []}, "actor": "test"}'

# Test SPARQL query
curl -X POST http://localhost:8090/v1/query \
  -H "Content-Type: application/json" \
  -d '{"query": "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 1", "kind": "sparql-select"}'
```

### Performance Testing

```bash
# Test basic throughput
ab -n 1000 -c 10 http://localhost:8090/healthz

# Test memory usage
ps aux | grep knowd

# Test disk usage
du -sh /var/lib/knowd
```

## Troubleshooting Installation

### Common Issues

#### Bazel Installation Problems
```bash
# Check Bazel version
bazel --version

# If version < 7.0, upgrade:
curl -fsSL https://bazel.build/bazel-release.pub.gpg | gpg --dearmor > bazel.gpg
sudo mv bazel.gpg /etc/apt/trusted.gpg.d/
echo "deb [arch=amd64] https://storage.googleapis.com/bazel-apt stable jdk1.8" | sudo tee /etc/apt/sources.list.d/bazel.list
sudo apt update
sudo apt upgrade bazel
```

#### Build Failures
```bash
# Clean build cache
bazel clean --expunge

# Rebuild with verbose output
bazel build //cmd/knowd:knowd --verbose_failures

# Check system dependencies
ldd bazel-bin/cmd/knowd/knowd_/knowd
```

#### Permission Issues
```bash
# Fix data directory permissions
sudo chown -R $(whoami):$(whoami) /var/lib/knowd

# Check binary permissions
chmod +x bazel-bin/cmd/knowd/knowd_/knowd
```

#### Port Conflicts
```bash
# Check if port is in use
netstat -tulpn | grep 8090

# Use different port
./knowd -addr :9090
```

### Debug Mode

```bash
# Enable debug logging
KNOWD_LOG_LEVEL=debug ./knowd

# Enable profiler
KNOWD_PPROF_ADDR=:6060 ./knowd

# Access profile data
go tool pprof http://localhost:6060/debug/pprof/profile
```

### Getting Help

- **GitHub Issues**: [Report installation issues](https://github.com/unrdf/knowd/issues)
- **Documentation**: Check [Configuration Guide](../getting-started/configuration.md)
- **Community**: Join [Discussions](https://github.com/unrdf/knowd/discussions)

This comprehensive installation guide covers all major platforms and deployment scenarios for Knowd knowledge graph database.

# Installation Guide

This guide covers various installation methods for Knowd, from development to production deployments.

## System Requirements

### Minimum Requirements
- **Operating System**: Linux, macOS, or Windows (WSL2 recommended for Windows)
- **Memory**: 2GB RAM minimum, 4GB recommended for production
- **Storage**: 1GB free space minimum, more for data storage
- **Network**: TCP port 8090 (configurable)

### Dependencies
- **Bazel 7.0+** - Build system
- **Go 1.22+** - Programming language (automatically downloaded by Bazel)
- **Git** - Version control

## Installation Methods

### Method 1: Bazel Build (Recommended for Development)

#### 1. Install Bazel

**Linux (Ubuntu/Debian):**
```bash
# Download Bazelisk (recommended Bazel wrapper)
curl -Lo bazel https://github.com/bazelbuild/bazelisk/releases/latest/download/bazelisk-linux-amd64
chmod +x bazel
sudo mv bazel /usr/local/bin/

# Or install Bazel directly
wget https://github.com/bazelbuild/bazel/releases/download/7.0.0/bazel-7.0.0-linux-x86_64
chmod +x bazel-7.0.0-linux-x86_64
sudo mv bazel-7.0.0-linux-x86_64 /usr/local/bin/bazel
```

**macOS:**
```bash
# Using Homebrew
brew install bazelisk

# Or Bazel directly
brew install bazel
```

**Windows (WSL2):**
```bash
# Install via Chocolatey
choco install bazel

# Or download manually
curl -Lo bazel.exe https://github.com/bazelbuild/bazel/releases/download/7.0.0/bazel-7.0.0-windows-x86_64.exe
```

#### 2. Clone and Build

```bash
# Clone the repository
git clone https://github.com/unrdf/knowd.git
cd knowd

# Build the binary
bazel build //cmd/knowd:knowd

# Run tests
bazel test //...

# Run the server
bazel run //cmd/knowd:knowd -- -addr :8090
```

### Method 2: Docker (Recommended for Production)

#### Using Pre-built Images

```bash
# Pull the latest image
docker pull unrdf/knowd:latest

# Run with persistent storage
docker run -d \
  --name knowd \
  -p 8090:8090 \
  -v knowd-data:/data \
  -e KNOWD_ADDR=:8090 \
  -e KNOWD_DATA_DIR=/data \
  unrdf/knowd:latest
```

#### Building Custom Image

```dockerfile
FROM golang:1.22-alpine AS builder

# Install Bazel
RUN wget -O /usr/local/bin/bazel https://github.com/bazelbuild/bazelisk/releases/latest/download/bazelisk-linux-amd64 && \
    chmod +x /usr/local/bin/bazel

# Copy source code
WORKDIR /app
COPY . .

# Build the binary
RUN bazel build //cmd/knowd:knowd

FROM alpine:latest
RUN apk --no-cache add ca-certificates
WORKDIR /root/
COPY --from=builder /app/bazel-bin/cmd/knowd/knowd_/knowd .
EXPOSE 8090
CMD ["./knowd", "-addr", ":8090"]
```

### Method 3: Binary Release

#### Download Pre-built Binaries

```bash
# Download for your platform
# Linux x86_64
wget https://github.com/unrdf/knowd/releases/latest/download/knowd-linux-amd64.tar.gz

# macOS x86_64
wget https://github.com/unrdf/knowd/releases/latest/download/knowd-darwin-amd64.tar.gz

# macOS ARM64
wget https://github.com/unrdf/knowd/releases/latest/download/knowd-darwin-arm64.tar.gz

# Extract and run
tar -xzf knowd-*.tar.gz
cd knowd-*/
./knowd -addr :8090
```

### Method 4: From Source (Advanced)

#### 1. Install Go

```bash
# Using goenv (recommended)
git clone https://github.com/go-nv/goenv.git ~/.goenv
echo 'export GOENV_ROOT="$HOME/.goenv"' >> ~/.bashrc
echo 'export PATH="$GOENV_ROOT/bin:$PATH"' >> ~/.bashrc
echo 'eval "$(goenv init -)"' >> ~/.bashrc
exec "$SHELL"

goenv install 1.22.0
goenv global 1.22.0

# Verify installation
go version
```

#### 2. Install Bazel Dependencies

```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install -y build-essential openjdk-11-jdk python3

# CentOS/RHEL
sudo yum groupinstall -y "Development Tools"
sudo yum install -y java-11-openjdk python3

# macOS (with Homebrew)
brew install bazelisk
```

#### 3. Build from Source

```bash
# Clone repository
git clone https://github.com/unrdf/knowd.git
cd knowd

# Initialize Bazel modules
bazel mod tidy

# Build binary
bazel build //cmd/knowd:knowd

# Run server
./bazel-bin/cmd/knowd/knowd_/knowd -addr :8090
```

## Verification

After installation, verify Knowd is working:

```bash
# Health check
curl http://localhost:8090/healthz
# Expected: ok

# Version info
curl http://localhost:8090/version
# Expected: Version and commit information

# Basic transaction (may be stub in early versions)
curl -X POST http://localhost:8090/v1/tx \
  -H "Content-Type: application/json" \
  -d '{"delta": {"add": []}, "actor": "test"}'
```

## Configuration

See the [Configuration Guide](./configuration.md) for detailed configuration options.

## Troubleshooting

### Common Issues

**"bazel: command not found"**
- Ensure Bazel/Bazelisk is in your PATH
- On Linux/macOS: `export PATH="/usr/local/bin:$PATH"`
- On Windows: Add to system PATH

**"go: command not found"**
- Bazel should handle Go automatically, but ensure Go is installed if building manually

**"Permission denied"**
- Ensure you have execute permissions on the binary
- On Linux: `chmod +x knowd`

**"Port 8090 already in use"**
- Use a different port: `./knowd -addr :9090`
- Find what's using the port: `lsof -i :8090` or `netstat -tulpn | grep :8090`

**Build fails with dependency errors**
- Clean build cache: `bazel clean --expunge`
- Update dependencies: `bazel mod tidy`

### Getting Help

- Check the [Troubleshooting Guide](../../troubleshooting/common-issues.md)
- File issues on [GitHub](https://github.com/unrdf/knowd/issues)
- Join discussions on [GitHub Discussions](https://github.com/unrdf/knowd/discussions)

## Next Steps

- **[Quick Start](./quick-start.md)** - Get running in minutes
- **[Configuration Guide](./configuration.md)** - Customize for your needs
- **[API Reference](../user-guide/api-reference.md)** - Explore available endpoints

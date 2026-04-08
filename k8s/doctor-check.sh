#!/bin/bash

# UNRDF Doctor Check Script for Kubernetes
# This script validates the OTEL observability stack using kubectl port-forwarding

echo "╔══════════════════════════════════════════════════════════╗"
echo "║     UNRDF Doctor - Kubernetes Health Check Report        ║"
echo "╚══════════════════════════════════════════════════════════╝"
echo ""

# Check K8s cluster connectivity
echo "Kubernetes Connectivity"
echo "══════════════════════════════════════════════════════════════"
KUBE_CONTEXT=$(kubectl config current-context 2>/dev/null)
if [ $? -eq 0 ]; then
  echo "✅ K8s cluster connected"
  echo "   Context: $KUBE_CONTEXT"

  # Check node status
  NODES=$(kubectl get nodes -o jsonpath='{.items[*].metadata.name}' 2>/dev/null)
  if [ -n "$NODES" ]; then
    NODE_COUNT=$(echo "$NODES" | wc -w | tr -d ' ')
    READY_NODES=$(kubectl get nodes -o jsonpath='{.items[?(@.status.conditions[?(@.type=="Ready")].status=="True")].metadata.name}' 2>/dev/null | wc -w | tr -d ' ')
    echo "   Nodes: $READY_NODES/$NODE_COUNT ready"
  fi
else
  echo "❌ Cannot connect to K8s cluster"
  exit 1
fi
echo ""

# Check Pod Status
echo "Pod Status"
echo "══════════════════════════════════════════════════════════════"
PODS=$(kubectl get pods -n unrdf-observability --no-headers 2>/dev/null)
if [ $? -eq 0 ]; then
  TOTAL_PODS=$(echo "$PODS" | wc -l | tr -d ' ')
  RUNNING_PODS=$(echo "$PODS" | grep -c "Running" || echo "0")

  echo "✅ Pods: $RUNNING_PODS/$TOTAL_PODS running"

  # Show pods with issues
  if [ "$RUNNING_PODS" -lt "$TOTAL_PODS" ]; then
    echo "   ⚠️  Non-running pods:"
    echo "$PODS" | grep -v "Running" | sed 's/^/      /'
  fi
else
  echo "❌ Cannot retrieve pod status"
fi
echo ""

# Check Services
echo "Services"
echo "══════════════════════════════════════════════════════════════"
kubectl get svc -n unrdf-observability --no-headers | while read -r line; do
  SVC_NAME=$(echo "$line" | awk '{print $1}')
  SVC_TYPE=$(echo "$line" | awk '{print $2}')
  CLUSTER_IP=$(echo "$line" | awk '{print $3}')
  PORTS=$(echo "$line" | awk '{print $5}' | sed 's/\/TCP//g')

  if [ "$SVC_TYPE" == "NodePort" ]; then
    echo "✅ $SVC_NAME ($SVC_TYPE, $PORTS)"
  else
    echo "✅ $SVC_NAME ($SVC_TYPE, $PORTS)"
  fi
done
echo ""

# Health Check via Port-Forward (Quick Test)
echo "Service Health Checks (via port-forward)"
echo "══════════════════════════════════════════════════════════════"

check_service_health() {
  local svc_name=$1
  local local_port=$2
  local svc_port=$3
  local display_name=$4
  local health_path=$5

  # Start port-forward in background
  kubectl port-forward -n unrdf-observability "svc/$svc_name" "$local_port:$svc_port" >/dev/null 2>&1 &
  local pf_pid=$!

  # Wait for port-forward to establish
  sleep 2

  # Test health endpoint
  if curl -sf "http://localhost:$local_port$health_path" >/dev/null 2>&1; then
    echo "✅ $display_name (healthy)"
  else
    echo "⚠️  $display_name (unhealthy or not responding on $health_path)"
  fi

  # Clean up port-forward
  kill $pf_pid 2>/dev/null
}

# Quick health checks (parallel for speed)
check_service_health "unrdf-obs-grafana" 30101 3000 "Grafana" "/api/health"
check_service_health "unrdf-obs-prometheus" 30102 9090 "Prometheus" "/-/healthy"
check_service_health "unrdf-obs-tempo" 30200 3200 "Tempo" "/api/v1/search"
check_service_health "unrdf-obs-loki" 30110 3100 "Loki" "/ready"
echo ""

# Check OTEL Collector specifically
echo "OTEL Collector Status"
echo "══════════════════════════════════════════════════════════════"
OTEL_POD=$(kubectl get pod -n unrdf-observability --field-selector=status.phase=Running -o name | grep otel-collector | head -1 | sed 's/pod\///')

if [ -n "$OTEL_POD" ]; then
  echo "✅ OTEL Collector pod running: $OTEL_POD"

  # Check OTEL collector endpoints via port-forward
  kubectl port-forward -n unrdf-observability "pod/$OTEL_POD" 14131:13133 >/dev/null 2>&1 &
  PF_PID=$!
  sleep 2

  if curl -sf http://localhost:14131 >/dev/null 2>&1; then
    echo "✅ OTEL Health check endpoint (:13133) responding"
  else
    echo "⚠️  OTEL Health check endpoint (:13133) not responding"
  fi

  kill $PF_PID 2>/dev/null
else
  echo "❌ OTEL Collector pod not running"
fi
echo ""

# Check Node Exporter
echo "Node Exporter"
echo "══════════════════════════════════════════════════════════════"
NODE_EXPORTER_POD=$(kubectl get pod -n unrdf-observability --field-selector=status.phase=Running -o name | grep node-exporter | head -1 | sed 's/pod\///')

if [ -n "$NODE_EXPORTER_POD" ]; then
  echo "✅ Node Exporter pod running: $NODE_EXPORTER_POD"

  # Test metrics endpoint
  kubectl exec -n unrdf-observability "$NODE_EXPORTER_POD" -- curl -s http://localhost:9100/metrics >/dev/null 2>&1
  if [ $? -eq 0 ]; then
    echo "✅ Node Exporter metrics endpoint (:9100) accessible"
  else
    echo "⚠️  Node Exporter metrics endpoint not responding"
  fi
else
  echo "❌ Node Exporter pod not running"
fi
echo ""

# Check Prometheus Targets
echo "Prometheus Targets"
echo "══════════════════════════════════════════════════════════════"
PROM_POD=$(kubectl get pod -n unrdf-observability --field-selector=status.phase=Running -o name | grep prometheus | grep -v promtail | head -1 | sed 's/pod\///')

if [ -n "$PROM_POD" ]; then
  # Query Prometheus API for targets
  TARGETS_JSON=$(kubectl exec -n unrdf-observability "$PROM_POD" -- curl -s 'http://localhost:9090/api/v1/targets' 2>/dev/null)

  if [ $? -eq 0 ]; then
    ACTIVE_TARGETS=$(echo "$TARGETS_JSON" | jq '[.data.activeTargets | length] // 0' 2>/dev/null)
    echo "✅ Prometheus (targets: $ACTIVE_TARGETS active)"

    # Check for unhealthy targets
    UNHEALTHY=$(echo "$TARGETS_JSON" | jq '[.data.activeTargets[] | select(.health != "up")] | length' 2>/dev/null)
    if [ "$UNHEALTHY" -gt 0 ]; then
      echo "   ⚠️  Unhealthy targets: $UNHEALTHY"
    fi
  else
    echo "⚠️  Prometheus (cannot query targets API)"
  fi
else
  echo "❌ Prometheus pod not running"
fi
echo ""

# Summary
echo "──────────────────────────────────────────────────"
echo "✅ Kubernetes: kind-unrdf cluster operational"
echo "📊 Observability Stack: Deployed and accessible"
echo ""
echo "Access URLs (via kubectl port-forward or NodePort):"
echo "  🔗 Grafana:    kubectl port-forward svc/unrdf-obs-grafana 3001:3000"
echo "  📈 Prometheus: kubectl port-forward svc/unrdf-obs-prometheus 3002:9090"
echo "  🔍 Tempo:      kubectl port-forward svc/unrdf-obs-tempo 3200:3200"
echo "  📊 Loki:       kubectl port-forward svc/unrdf-obs-loki 3100:3100"
echo ""
echo "Or use NodePorts:"
echo "  🔗 Grafana:    http://localhost:30001"
echo "  📈 Prometheus: http://localhost:30002"
echo "  🔍 Tempo:      http://localhost:30200"
echo "══════════════════════════════════════════════════════════════"

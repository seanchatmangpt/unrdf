"use client";

import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";

export function PolicySecurityDemo() {
  const [policies, setPolicies] = useState([
    { name: "PersonShape", status: "active", violations: 0 },
    { name: "ProductShape", status: "active", violations: 2 },
    { name: "OrderShape", status: "disabled", violations: 0 },
  ]);
  const [auditLog, setAuditLog] = useState([]);
  const [sandboxResult, setSandboxResult] = useState(null);

  const checkAccess = (action) => {
    const allowed = action !== "delete";
    setAuditLog(prev => [...prev, {
      id: Date.now(),
      user: "alice@example.org",
      action,
      resource: "http://example.org/data",
      allowed,
      timestamp: new Date().toISOString()
    }]);
  };

  const runSandboxQuery = () => {
    setSandboxResult({
      success: true,
      results: 42,
      executionTime: 156,
      memoryUsed: "2.4MB",
      timedOut: false
    });
  };

  return (
    <div className="space-y-6">
      {/* usePolicyPack */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-sm font-medium text-slate-300 mb-3">usePolicyPack</div>
        <div className="text-xs text-slate-400 mb-3">Manage SHACL policy packs for validation</div>
        <div className="space-y-2">
          {policies.map((p, i) => (
            <div key={i} className="flex items-center justify-between bg-slate-800 p-2 rounded">
              <span className="text-sm text-slate-300">{p.name}</span>
              <div className="flex items-center gap-2">
                <Badge variant={p.status === "active" ? "success" : "secondary"}>
                  {p.status}
                </Badge>
                {p.violations > 0 && (
                  <Badge variant="destructive">{p.violations} violations</Badge>
                )}
              </div>
            </div>
          ))}
        </div>
      </div>

      {/* useSecurityValidator */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-sm font-medium text-slate-300 mb-3">useSecurityValidator</div>
        <div className="text-xs text-slate-400 mb-3">Access control and audit logging</div>

        <div className="flex gap-2 mb-4">
          <Button variant="outline" size="sm" onClick={() => checkAccess("read")}>
            Check Read
          </Button>
          <Button variant="outline" size="sm" onClick={() => checkAccess("write")}>
            Check Write
          </Button>
          <Button variant="destructive" size="sm" onClick={() => checkAccess("delete")}>
            Check Delete
          </Button>
        </div>

        <div className="bg-slate-800 rounded p-3 max-h-32 overflow-y-auto">
          <div className="text-xs text-slate-500 mb-2">Audit Log</div>
          {auditLog.length === 0 ? (
            <div className="text-center text-slate-500 py-2 text-xs">No audit entries</div>
          ) : (
            <div className="space-y-1">
              {auditLog.slice(-5).reverse().map((log) => (
                <div key={log.id} className="flex items-center justify-between text-xs">
                  <span className="text-slate-400">{log.user}</span>
                  <span className="text-slate-300">{log.action}</span>
                  <Badge variant={log.allowed ? "success" : "destructive"} className="text-xs">
                    {log.allowed ? "ALLOWED" : "DENIED"}
                  </Badge>
                </div>
              ))}
            </div>
          )}
        </div>
      </div>

      {/* useSandbox */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="flex items-center justify-between mb-3">
          <div>
            <div className="text-sm font-medium text-slate-300">useSandbox</div>
            <div className="text-xs text-slate-400">Isolated query execution with limits</div>
          </div>
          <Button variant="outline" size="sm" onClick={runSandboxQuery}>
            Execute Sandboxed
          </Button>
        </div>

        <div className="grid grid-cols-4 gap-2 mb-3">
          <div className="bg-slate-800 p-2 rounded text-center">
            <div className="text-xs text-slate-500">Timeout</div>
            <div className="text-sm font-medium text-slate-300">30s</div>
          </div>
          <div className="bg-slate-800 p-2 rounded text-center">
            <div className="text-xs text-slate-500">Max Results</div>
            <div className="text-sm font-medium text-slate-300">1000</div>
          </div>
          <div className="bg-slate-800 p-2 rounded text-center">
            <div className="text-xs text-slate-500">Memory Limit</div>
            <div className="text-sm font-medium text-slate-300">50MB</div>
          </div>
          <div className="bg-slate-800 p-2 rounded text-center">
            <div className="text-xs text-slate-500">Isolation</div>
            <div className="text-sm font-medium text-green-400">ON</div>
          </div>
        </div>

        {sandboxResult && (
          <div className="bg-slate-800 rounded p-3">
            <div className="flex items-center gap-4 text-xs">
              <Badge variant="success">Completed</Badge>
              <span className="text-slate-400">{sandboxResult.results} results</span>
              <span className="text-slate-400">{sandboxResult.executionTime}ms</span>
              <span className="text-slate-400">{sandboxResult.memoryUsed}</span>
            </div>
          </div>
        )}
      </div>

      {/* Code Example */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-xs text-slate-500 mb-2">Usage Example - All Policy & Security Hooks</div>
        <pre className="text-xs text-slate-300 overflow-x-auto">
{`import {
  usePolicyPack,
  useSecurityValidator,
  useSandbox
} from 'unrdf/react-hooks';

// SHACL policy management
const { loadPolicy, validate, violations, policies } = usePolicyPack();
await loadPolicy('https://example.org/shapes/person.ttl');
const result = await validate(dataGraph);

// Access control with audit trail
const { checkAccess, auditLog, permissions } = useSecurityValidator();
const allowed = await checkAccess('alice', 'http://example.org/data', 'write');
// Audit log automatically updated

// Sandboxed query execution
const { execute, abort, status } = useSandbox({
  timeout: 30000,
  maxResults: 1000,
  memoryLimit: '50MB'
});
const result = await execute('SELECT * WHERE { ?s ?p ?o }');
// Auto-aborts if timeout exceeded`}
        </pre>
      </div>
    </div>
  );
}

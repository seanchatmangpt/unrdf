"use client"

import * as React from "react"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert"
import { Badge } from "@/components/ui/badge"
import { Button } from "@/components/ui/button"
import { Select } from "@/components/ui/select"
import { Separator } from "@/components/ui/separator"
import { AlertCircle, Clock, User, Shield } from "lucide-react"

export default function HealthcareDemo() {
  const [selectedTimestamp, setSelectedTimestamp] = React.useState("current")
  const [reconstructedState, setReconstructedState] = React.useState(null)

  // Simulated patient medication record with time-travel events
  const events = [
    {
      t_ns: "1704657600000000000", // 2024-01-07 16:00:00 UTC
      timestamp: "2024-01-07 16:00:00",
      type: "MEDICATION_PRESCRIBED",
      user: "Dr. Sarah Chen",
      userId: "MD-8472",
      delta: { medication: "Lisinopril", dosage: "10mg", frequency: "once daily" },
      vectorClock: { "node-1": 1 },
      hash: "blake3:a7f8d9e2c1b4..."
    },
    {
      t_ns: "1704744000000000000", // 2024-01-08 16:00:00 UTC
      timestamp: "2024-01-08 16:00:00",
      type: "MEDICATION_UPDATED",
      user: "Dr. Michael Torres",
      userId: "MD-3391",
      delta: { medication: "Lisinopril", dosage: "20mg", frequency: "once daily", reason: "Inadequate BP control" },
      vectorClock: { "node-1": 2 },
      hash: "blake3:b2c9e4f1d3a8..."
    },
    {
      t_ns: "1704830400000000000", // 2024-01-09 16:00:00 UTC
      timestamp: "2024-01-09 16:00:00",
      type: "MEDICATION_UPDATED",
      user: "Pharmacy System",
      userId: "SYS-AUTO",
      delta: { medication: "Lisinopril", dosage: "10mg", frequency: "once daily", reason: "Insurance formulary reversion" },
      vectorClock: { "node-1": 3 },
      hash: "blake3:c4d1a5e7f2b9..."
    },
    {
      t_ns: "1704916800000000000", // 2024-01-10 16:00:00 UTC
      timestamp: "2024-01-10 16:00:00",
      type: "AUDIT_ALERT",
      user: "Quality Assurance Team",
      userId: "QA-5129",
      delta: { finding: "Patient received incorrect 10mg dosage for 3 days", severity: "HIGH" },
      vectorClock: { "node-1": 4 },
      hash: "blake3:d7e3b2c9a1f4..."
    }
  ]

  const timelineOptions = [
    { value: "current", label: "Current State (2024-01-10 16:00:00)" },
    { value: "1704830400000000000", label: "Before Reversion (2024-01-09 16:00:00)" },
    { value: "1704744000000000000", label: "After Dosage Increase (2024-01-08 16:00:00)" },
    { value: "1704657600000000000", label: "Initial Prescription (2024-01-07 16:00:00)" }
  ]

  const handleReconstruct = () => {
    // Simulate 4D time-travel reconstruction
    if (selectedTimestamp === "current") {
      setReconstructedState({
        medication: "Lisinopril",
        dosage: "10mg",
        frequency: "once daily",
        lastModified: "2024-01-09 16:00:00",
        modifiedBy: "SYS-AUTO",
        reason: "Insurance formulary reversion"
      })
    } else {
      const event = events.find(e => e.t_ns === selectedTimestamp)
      if (event) {
        setReconstructedState({
          medication: event.delta.medication,
          dosage: event.delta.dosage,
          frequency: event.delta.frequency,
          lastModified: event.timestamp,
          modifiedBy: event.user,
          reason: event.delta.reason || "Initial prescription"
        })
      }
    }
  }

  return (
    <div className="space-y-6">
      <div>
        <h2 className="text-3xl font-bold tracking-tight">Healthcare: HIPAA Patient Records</h2>
        <p className="text-muted-foreground mt-2">
          Reconstruct exact patient state at any point in time for compliance and audit trail validation.
        </p>
      </div>

      <Alert variant="warning">
        <AlertCircle className="h-4 w-4" />
        <AlertTitle>Audit Finding: Incorrect Dosage</AlertTitle>
        <AlertDescription>
          Patient #47293 received incorrect 10mg dosage of Lisinopril for 3 days (Jan 9-10, 2024)
          despite physician order for 20mg on Jan 8. Time-travel reconstruction proves pharmacy
          system auto-reverted dosage due to insurance formulary rules.
        </AlertDescription>
      </Alert>

      <Card>
        <CardHeader>
          <CardTitle>4D Time-Travel Reconstruction</CardTitle>
          <CardDescription>
            Select a timestamp to reconstruct the exact patient record state with full audit trail
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="flex gap-4">
            <Select
              value={selectedTimestamp}
              onChange={(e) => setSelectedTimestamp(e.target.value)}
              className="flex-1"
            >
              {timelineOptions.map(opt => (
                <option key={opt.value} value={opt.value}>{opt.label}</option>
              ))}
            </Select>
            <Button onClick={handleReconstruct}>
              <Clock className="mr-2 h-4 w-4" />
              Reconstruct State
            </Button>
          </div>

          {reconstructedState && (
            <div className="mt-6 space-y-4">
              <Separator />
              <div className="bg-muted/50 rounded-lg p-4 space-y-3">
                <div className="flex items-center gap-2">
                  <Shield className="h-5 w-5 text-blue-500" />
                  <h3 className="font-semibold">Reconstructed Patient Record</h3>
                  <Badge variant="outline">HIPAA Compliant</Badge>
                </div>

                <div className="grid grid-cols-2 gap-4 text-sm">
                  <div>
                    <div className="text-muted-foreground">Medication</div>
                    <div className="font-medium">{reconstructedState.medication}</div>
                  </div>
                  <div>
                    <div className="text-muted-foreground">Dosage</div>
                    <div className="font-medium text-lg">{reconstructedState.dosage}</div>
                  </div>
                  <div>
                    <div className="text-muted-foreground">Frequency</div>
                    <div className="font-medium">{reconstructedState.frequency}</div>
                  </div>
                  <div>
                    <div className="text-muted-foreground">Last Modified</div>
                    <div className="font-medium">{reconstructedState.lastModified}</div>
                  </div>
                  <div className="col-span-2">
                    <div className="text-muted-foreground">Modified By</div>
                    <div className="font-medium flex items-center gap-2">
                      <User className="h-4 w-4" />
                      {reconstructedState.modifiedBy}
                    </div>
                  </div>
                  <div className="col-span-2">
                    <div className="text-muted-foreground">Reason</div>
                    <div className="font-medium">{reconstructedState.reason}</div>
                  </div>
                </div>
              </div>
            </div>
          )}
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Complete Event Timeline</CardTitle>
          <CardDescription>Immutable audit trail with vector clocks and BLAKE3 integrity hashes</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="space-y-3">
            {events.map((event, idx) => (
              <div key={event.t_ns} className="flex gap-4 pb-3 border-b last:border-0">
                <div className="flex-shrink-0 w-40 text-sm text-muted-foreground">
                  {event.timestamp}
                </div>
                <div className="flex-1 space-y-1">
                  <div className="flex items-center gap-2">
                    <Badge variant={event.type === "AUDIT_ALERT" ? "destructive" : "default"}>
                      {event.type}
                    </Badge>
                    <span className="text-sm font-medium">{event.user}</span>
                  </div>
                  <div className="text-sm text-muted-foreground">
                    {event.delta.dosage && `Dosage: ${event.delta.dosage}`}
                    {event.delta.finding && event.delta.finding}
                  </div>
                  <div className="text-xs font-mono text-muted-foreground">
                    Vector Clock: {JSON.stringify(event.vectorClock)} | Hash: {event.hash}
                  </div>
                </div>
              </div>
            ))}
          </div>
        </CardContent>
      </Card>

      <Alert variant="success">
        <Shield className="h-4 w-4" />
        <AlertTitle>Compliance Benefits</AlertTitle>
        <AlertDescription>
          <ul className="list-disc list-inside space-y-1 mt-2">
            <li>HIPAA audit trail: Prove exact state at any timestamp with cryptographic integrity</li>
            <li>Root cause analysis: Insurance system auto-reversion identified in &lt;5 seconds</li>
            <li>Legal defensibility: Immutable event log prevents tampering, BLAKE3 hashes verify authenticity</li>
            <li>Concurrent updates: Vector clocks track causality across distributed EHR systems</li>
          </ul>
        </AlertDescription>
      </Alert>
    </div>
  )
}

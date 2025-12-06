"use client"

import * as React from "react"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert"
import { Badge } from "@/components/ui/badge"
import { Button } from "@/components/ui/button"
import { Select } from "@/components/ui/select"
import { Separator } from "@/components/ui/separator"
import { AlertCircle, Clock, ShieldAlert, CheckCircle } from "lucide-react"

export default function ECommerceDemo() {
  const [selectedTimestamp, setSelectedTimestamp] = React.useState("current")
  const [reconstructedState, setReconstructedState] = React.useState(null)

  // Simulated e-commerce fraud detection with concurrent operations
  const events = [
    {
      t_ns: "1704808800000000000", // 2024-01-09 14:00:00 UTC
      timestamp: "2024-01-09 14:00:00",
      type: "ORDER_PLACED",
      actor: "Customer (user-8472)",
      actorId: "user-8472",
      delta: {
        orderId: "ORD-847291",
        amount: 1249.99,
        shippingAddress: "123 Main St, Austin, TX 78701",
        billingAddress: "123 Main St, Austin, TX 78701",
        ipAddress: "72.14.192.5"
      },
      vectorClock: { "web-1": 1, "inventory": 0, "fraud": 0 },
      hash: "blake3:d8e4f2a9c1b7..."
    },
    {
      t_ns: "1704808860000000000", // 2024-01-09 14:01:00 UTC (1 minute later)
      timestamp: "2024-01-09 14:01:00",
      type: "PAYMENT_AUTHORIZED",
      actor: "Payment Gateway",
      actorId: "SYS-PAYMENT",
      delta: {
        orderId: "ORD-847291",
        paymentId: "PAY-193847",
        status: "AUTHORIZED",
        cvvMatch: true,
        avsMatch: true
      },
      vectorClock: { "web-1": 1, "inventory": 0, "fraud": 0, "payment": 1 },
      hash: "blake3:a1c7e9f4d2b3..."
    },
    {
      t_ns: "1704808920000000000", // 2024-01-09 14:02:00 UTC (2 minutes after order)
      timestamp: "2024-01-09 14:02:00",
      type: "SHIPPING_ADDRESS_UPDATED",
      actor: "Customer (user-8472)",
      actorId: "user-8472",
      delta: {
        orderId: "ORD-847291",
        oldAddress: "123 Main St, Austin, TX 78701",
        newAddress: "987 Oak Ave, Miami, FL 33139",
        ipAddress: "198.51.100.42",
        reason: "Customer requested change"
      },
      vectorClock: { "web-1": 2, "inventory": 0, "fraud": 0, "payment": 1 },
      hash: "blake3:c9e2f7a4d1b8..."
    },
    {
      t_ns: "1704809100000000000", // 2024-01-09 14:05:00 UTC (5 minutes after order)
      timestamp: "2024-01-09 14:05:00",
      type: "FRAUD_ALERT",
      actor: "Fraud Detection System",
      actorId: "SYS-FRAUD",
      delta: {
        orderId: "ORD-847291",
        riskScore: 87,
        flags: [
          "Shipping address changed within 5 minutes",
          "New IP address different geographic region",
          "High-value order ($1,249.99)"
        ],
        recommendation: "HOLD_FOR_REVIEW"
      },
      vectorClock: { "web-1": 2, "inventory": 0, "fraud": 1, "payment": 1 },
      hash: "blake3:e7f4a2c9d1b3..."
    },
    {
      t_ns: "1704809280000000000", // 2024-01-09 14:08:00 UTC (8 minutes after order)
      timestamp: "2024-01-09 14:08:00",
      type: "ORDER_HELD",
      actor: "Risk Management Team",
      actorId: "RISK-3391",
      delta: {
        orderId: "ORD-847291",
        status: "HELD",
        reason: "Fraud review: Suspicious address change pattern",
        customerNotified: true
      },
      vectorClock: { "web-1": 2, "inventory": 0, "fraud": 1, "payment": 1, "risk": 1 },
      hash: "blake3:f2b9e7c4a1d8..."
    },
    {
      t_ns: "1704823680000000000", // 2024-01-09 18:08:00 UTC (4 hours later)
      timestamp: "2024-01-09 18:08:00",
      type: "CHARGEBACK_DISPUTE",
      actor: "Customer (user-8472)",
      actorId: "user-8472",
      delta: {
        orderId: "ORD-847291",
        claim: "Item never shipped, database shows address I never authorized",
        disputeAmount: 1249.99
      },
      vectorClock: { "web-1": 3, "inventory": 0, "fraud": 1, "payment": 1, "risk": 1 },
      hash: "blake3:b8d4e1f7c2a9..."
    }
  ]

  const timelineOptions = [
    { value: "current", label: "Current State (Chargeback Dispute)" },
    { value: "1704809280000000000", label: "Order Held (14:08:00)" },
    { value: "1704809100000000000", label: "Fraud Alert (14:05:00)" },
    { value: "1704808920000000000", label: "Address Changed (14:02:00)" },
    { value: "1704808860000000000", label: "Payment Authorized (14:01:00)" },
    { value: "1704808800000000000", label: "Order Placed (14:00:00)" }
  ]

  const handleReconstruct = () => {
    if (selectedTimestamp === "current") {
      setReconstructedState({
        orderId: "ORD-847291",
        status: "DISPUTED",
        shippingAddress: "987 Oak Ave, Miami, FL 33139",
        billingAddress: "123 Main St, Austin, TX 78701",
        riskScore: 87,
        timeline: "Address changed 2 minutes after order placement",
        proof: "Customer DID authorize change at 14:02:00 from IP 198.51.100.42"
      })
    } else {
      const targetTime = BigInt(selectedTimestamp)
      const relevantEvents = events.filter(e => BigInt(e.t_ns) <= targetTime)
      const lastEvent = relevantEvents[relevantEvents.length - 1]

      let state = {
        orderId: "ORD-847291",
        status: "PROCESSING",
        shippingAddress: "123 Main St, Austin, TX 78701",
        billingAddress: "123 Main St, Austin, TX 78701",
        riskScore: 0,
        timeline: "",
        proof: ""
      }

      relevantEvents.forEach(event => {
        if (event.type === "SHIPPING_ADDRESS_UPDATED") {
          state.shippingAddress = event.delta.newAddress
          state.timeline = "Address changed at " + event.timestamp
        }
        if (event.type === "FRAUD_ALERT") {
          state.riskScore = event.delta.riskScore
        }
        if (event.type === "ORDER_HELD") {
          state.status = "HELD"
        }
      })

      setReconstructedState(state)
    }
  }

  return (
    <div className="space-y-6">
      <div>
        <h2 className="text-3xl font-bold tracking-tight">E-Commerce: Fraud Detection & Dispute Resolution</h2>
        <p className="text-muted-foreground mt-2">
          Exact event sequencing for chargeback defense and fraud investigation.
        </p>
      </div>

      <Alert variant="destructive">
        <AlertCircle className="h-4 w-4" />
        <AlertTitle>Chargeback Dispute Filed</AlertTitle>
        <AlertDescription>
          Customer claims "item never shipped, database shows address I never authorized."
          Customer is lying: Time-travel reconstruction proves customer PERSONALLY changed
          shipping address at 14:02:00 UTC from verified IP address 198.51.100.42.
        </AlertDescription>
      </Alert>

      <Card>
        <CardHeader>
          <CardTitle>4D Time-Travel Fraud Investigation</CardTitle>
          <CardDescription>
            Reconstruct exact order state and event sequence to prove customer authorization
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
              Reconstruct Order State
            </Button>
          </div>

          {reconstructedState && (
            <div className="mt-6 space-y-4">
              <Separator />
              <div className="bg-muted/50 rounded-lg p-4 space-y-3">
                <div className="flex items-center gap-2">
                  <ShieldAlert className="h-5 w-5 text-orange-500" />
                  <h3 className="font-semibold">Reconstructed Order State</h3>
                  <Badge variant={reconstructedState.riskScore > 50 ? "destructive" : "default"}>
                    Risk Score: {reconstructedState.riskScore}
                  </Badge>
                </div>

                <div className="grid grid-cols-2 gap-4 text-sm">
                  <div>
                    <div className="text-muted-foreground">Order ID</div>
                    <div className="font-medium">{reconstructedState.orderId}</div>
                  </div>
                  <div>
                    <div className="text-muted-foreground">Status</div>
                    <div className="font-medium">
                      <Badge variant={reconstructedState.status === "DISPUTED" ? "destructive" : "secondary"}>
                        {reconstructedState.status}
                      </Badge>
                    </div>
                  </div>
                  <div className="col-span-2">
                    <div className="text-muted-foreground">Shipping Address</div>
                    <div className="font-medium">{reconstructedState.shippingAddress}</div>
                  </div>
                  <div className="col-span-2">
                    <div className="text-muted-foreground">Billing Address</div>
                    <div className="font-medium">{reconstructedState.billingAddress}</div>
                  </div>
                  {reconstructedState.timeline && (
                    <div className="col-span-2">
                      <div className="text-muted-foreground">Timeline Evidence</div>
                      <div className="font-medium">{reconstructedState.timeline}</div>
                    </div>
                  )}
                  {reconstructedState.proof && (
                    <div className="col-span-2 bg-green-50 dark:bg-green-950 p-3 rounded border border-green-200">
                      <div className="text-green-900 dark:text-green-100 font-semibold flex items-center gap-2">
                        <CheckCircle className="h-4 w-4" />
                        Chargeback Defense Proof
                      </div>
                      <div className="text-green-800 dark:text-green-200 text-sm mt-1">
                        {reconstructedState.proof}
                      </div>
                    </div>
                  )}
                </div>
              </div>
            </div>
          )}
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Complete Transaction Timeline</CardTitle>
          <CardDescription>
            Immutable event sequence with vector clocks proving causality
          </CardDescription>
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
                    <Badge variant={
                      event.type === "FRAUD_ALERT" ? "destructive" :
                      event.type === "ORDER_HELD" ? "warning" :
                      event.type === "CHARGEBACK_DISPUTE" ? "destructive" :
                      "default"
                    }>
                      {event.type}
                    </Badge>
                    <span className="text-sm font-medium">{event.actor}</span>
                  </div>
                  <div className="text-sm">
                    {event.delta.newAddress && (
                      <div>Address changed: <span className="font-medium">{event.delta.newAddress}</span></div>
                    )}
                    {event.delta.riskScore && (
                      <div>Risk Score: <span className="font-medium text-red-600">{event.delta.riskScore}</span></div>
                    )}
                    {event.delta.claim && (
                      <div className="italic text-muted-foreground">{event.delta.claim}</div>
                    )}
                    {event.delta.ipAddress && (
                      <div className="text-xs text-muted-foreground">IP: {event.delta.ipAddress}</div>
                    )}
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
        <CheckCircle className="h-4 w-4" />
        <AlertTitle>Chargeback Defense Won</AlertTitle>
        <AlertDescription>
          <ul className="list-disc list-inside space-y-1 mt-2">
            <li>Proof of customer authorization: Address change at 14:02:00 from customer's verified IP</li>
            <li>Event sequence: Change occurred BEFORE fraud alert (14:05:00) and order hold (14:08:00)</li>
            <li>Vector clocks: No race conditions, customer action definitely happened</li>
            <li>BLAKE3 integrity: Immutable log prevents tampering, customer cannot claim database corruption</li>
            <li>Result: Chargeback denied, merchant saves $1,249.99 + chargeback fees</li>
          </ul>
        </AlertDescription>
      </Alert>
    </div>
  )
}

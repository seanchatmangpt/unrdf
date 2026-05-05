"use client"

import * as React from "react"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert"
import { Badge } from "@/components/ui/badge"
import { Button } from "@/components/ui/button"
import { Select } from "@/components/ui/select"
import { Separator } from "@/components/ui/separator"
import { AlertCircle, Clock, TrendingUp, Shield } from "lucide-react"

export default function FinancialDemo() {
  const [selectedTimestamp, setSelectedTimestamp] = React.useState("current")
  const [reconstructedPosition, setReconstructedPosition] = React.useState(null)

  // Simulated trading position events with SOX/FINRA compliance
  const events = [
    {
      t_ns: "1704722400000000000", // 2024-01-08 10:00:00 UTC (Market Open)
      timestamp: "2024-01-08 10:00:00",
      type: "POSITION_OPEN",
      trader: "Portfolio Manager A",
      traderId: "PM-4821",
      delta: { symbol: "AAPL", quantity: 10000, price: 185.50, position: 10000, marketValue: 1855000 },
      vectorClock: { "trading-desk-1": 1, "risk-system": 0 },
      hash: "blake3:f4e2a1d9c7b3..."
    },
    {
      t_ns: "1704729600000000000", // 2024-01-08 12:00:00 UTC
      timestamp: "2024-01-08 12:00:00",
      type: "POSITION_INCREASE",
      trader: "Portfolio Manager B",
      traderId: "PM-7193",
      delta: { symbol: "AAPL", quantity: 5000, price: 186.25, position: 15000, marketValue: 2793750 },
      vectorClock: { "trading-desk-1": 2, "trading-desk-2": 1, "risk-system": 0 },
      hash: "blake3:a9c3e7f2d1b4..."
    },
    {
      t_ns: "1704736800000000000", // 2024-01-08 14:00:00 UTC
      timestamp: "2024-01-08 14:00:00",
      type: "POSITION_REDUCE",
      trader: "Risk Management System",
      traderId: "SYS-RISK",
      delta: { symbol: "AAPL", quantity: -3000, price: 187.00, position: 12000, marketValue: 2244000, reason: "Position limit breach" },
      vectorClock: { "trading-desk-1": 2, "trading-desk-2": 1, "risk-system": 1 },
      hash: "blake3:c7d4b2e9a1f3..."
    },
    {
      t_ns: "1704740400000000000", // 2024-01-08 15:00:00 UTC (CFTC Submission Time)
      timestamp: "2024-01-08 15:00:00",
      type: "REGULATORY_SNAPSHOT",
      trader: "Compliance System",
      traderId: "SYS-COMP",
      delta: { symbol: "AAPL", position: 12000, marketValue: 2244000, submitted: "CFTC", confirmationId: "CFTC-2024-01-08-8472" },
      vectorClock: { "trading-desk-1": 2, "trading-desk-2": 1, "risk-system": 1, "compliance": 1 },
      hash: "blake3:e2f9a7c4d1b8..."
    },
    {
      t_ns: "1704743999000000000", // 2024-01-08 15:59:59 UTC (Just before market close)
      timestamp: "2024-01-08 15:59:59",
      type: "LATE_TRADE_CORRECTION",
      trader: "Portfolio Manager B",
      traderId: "PM-7193",
      delta: { symbol: "AAPL", quantity: -500, price: 186.75, position: 14500, marketValue: 2707875, reason: "Trade entry error - correction submitted after CFTC snapshot" },
      vectorClock: { "trading-desk-1": 2, "trading-desk-2": 2, "risk-system": 1, "compliance": 1 },
      hash: "blake3:b4c9e1f7d2a3..."
    }
  ]

  const timelineOptions = [
    { value: "current", label: "Current State (End of Day)" },
    { value: "1704740400000000000", label: "CFTC Submission Time (15:00:00)" },
    { value: "1704736800000000000", label: "After Risk Reduction (14:00:00)" },
    { value: "1704729600000000000", label: "After PM-B Trade (12:00:00)" },
    { value: "1704722400000000000", label: "Market Open (10:00:00)" }
  ]

  const handleReconstruct = () => {
    if (selectedTimestamp === "current") {
      setReconstructedPosition({
        symbol: "AAPL",
        position: 14500,
        marketValue: 2707875,
        timestamp: "2024-01-08 15:59:59",
        source: "End of Day",
        discrepancy: 2100000 - 2244000 // $144k difference vs CFTC submission
      })
    } else {
      const targetTime = BigInt(selectedTimestamp)
      const relevantEvents = events.filter(e => BigInt(e.t_ns) <= targetTime)
      const lastEvent = relevantEvents[relevantEvents.length - 1]

      if (lastEvent) {
        setReconstructedPosition({
          symbol: lastEvent.delta.symbol,
          position: lastEvent.delta.position,
          marketValue: lastEvent.delta.marketValue,
          timestamp: lastEvent.timestamp,
          source: lastEvent.type,
          discrepancy: lastEvent.delta.position === 12000 ? 0 : lastEvent.delta.marketValue - 2244000
        })
      }
    }
  }

  return (
    <div className="space-y-6">
      <div>
        <h2 className="text-3xl font-bold tracking-tight">Financial Services: SOX Trading Compliance</h2>
        <p className="text-muted-foreground mt-2">
          Exact position reconstruction for regulatory submissions and audit defense.
        </p>
      </div>

      <Alert variant="destructive">
        <AlertCircle className="h-4 w-4" />
        <AlertTitle>Position Discrepancy Detected</AlertTitle>
        <AlertDescription>
          CFTC submission shows 12,000 shares ($2.24M) at 15:00:00, but internal records show
          14,500 shares ($2.71M) end-of-day. Time-travel proves late trade correction (-500 shares)
          was submitted at 15:59:59, AFTER regulatory snapshot. No fraud, just timing issue.
        </AlertDescription>
      </Alert>

      <Card>
        <CardHeader>
          <CardTitle>4D Time-Travel Position Reconstruction</CardTitle>
          <CardDescription>
            Reconstruct exact position at any nanosecond for SOX/FINRA compliance
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
              Reconstruct Position
            </Button>
          </div>

          {reconstructedPosition && (
            <div className="mt-6 space-y-4">
              <Separator />
              <div className="bg-muted/50 rounded-lg p-4 space-y-3">
                <div className="flex items-center gap-2">
                  <TrendingUp className="h-5 w-5 text-green-500" />
                  <h3 className="font-semibold">Reconstructed Position</h3>
                  <Badge variant="outline">SOX Compliant</Badge>
                </div>

                <div className="grid grid-cols-2 gap-4 text-sm">
                  <div>
                    <div className="text-muted-foreground">Symbol</div>
                    <div className="font-medium text-xl">{reconstructedPosition.symbol}</div>
                  </div>
                  <div>
                    <div className="text-muted-foreground">Position</div>
                    <div className="font-medium text-xl">
                      {reconstructedPosition.position.toLocaleString()} shares
                    </div>
                  </div>
                  <div>
                    <div className="text-muted-foreground">Market Value</div>
                    <div className="font-medium text-lg">
                      ${(reconstructedPosition.marketValue / 1000000).toFixed(2)}M
                    </div>
                  </div>
                  <div>
                    <div className="text-muted-foreground">Timestamp</div>
                    <div className="font-medium">{reconstructedPosition.timestamp}</div>
                  </div>
                  <div className="col-span-2">
                    <div className="text-muted-foreground">Source</div>
                    <div className="font-medium">{reconstructedPosition.source}</div>
                  </div>
                  {reconstructedPosition.discrepancy !== 0 && (
                    <div className="col-span-2">
                      <div className="text-muted-foreground">Discrepancy vs CFTC Submission</div>
                      <div className={`font-medium ${reconstructedPosition.discrepancy > 0 ? 'text-red-500' : 'text-green-500'}`}>
                        {reconstructedPosition.discrepancy > 0 ? '+' : ''}
                        ${(reconstructedPosition.discrepancy / 1000).toFixed(0)}K
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
          <CardTitle>Complete Trading Timeline</CardTitle>
          <CardDescription>
            Immutable audit trail with vector clocks proving causality and sequence
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
                    <Badge variant={event.type === "REGULATORY_SNAPSHOT" ? "default" : event.type === "LATE_TRADE_CORRECTION" ? "destructive" : "secondary"}>
                      {event.type}
                    </Badge>
                    <span className="text-sm font-medium">{event.trader}</span>
                  </div>
                  <div className="text-sm">
                    Position: <span className="font-medium">{event.delta.position?.toLocaleString()} shares</span>
                    {event.delta.quantity && (
                      <span className={event.delta.quantity > 0 ? "text-green-600 ml-2" : "text-red-600 ml-2"}>
                        ({event.delta.quantity > 0 ? '+' : ''}{event.delta.quantity?.toLocaleString()})
                      </span>
                    )}
                    {event.delta.reason && (
                      <span className="text-muted-foreground ml-2 italic">- {event.delta.reason}</span>
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
        <Shield className="h-4 w-4" />
        <AlertTitle>Regulatory Compliance Benefits</AlertTitle>
        <AlertDescription>
          <ul className="list-disc list-inside space-y-1 mt-2">
            <li>SOX/FINRA audit defense: Prove exact position at regulatory submission time (&lt;5s)</li>
            <li>Discrepancy resolution: Late trade correction identified with nanosecond precision</li>
            <li>No fraud accusation: Timeline proves timing issue, not intentional misreporting</li>
            <li>Distributed causality: Vector clocks prove no race conditions across 4 systems</li>
            <li>Legal defensibility: BLAKE3 hashes prevent tampering, immutable event log</li>
          </ul>
        </AlertDescription>
      </Alert>
    </div>
  )
}

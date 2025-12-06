"use client"

import * as React from "react"
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"
import HealthcareDemo from "@/components/use-cases/HealthcareDemo"
import FinancialDemo from "@/components/use-cases/FinancialDemo"
import ECommerceDemo from "@/components/use-cases/ECommerceDemo"

export default function UseCasesPage() {
  const [activeTab, setActiveTab] = React.useState("healthcare")

  return (
    <div className="container mx-auto p-8 max-w-7xl">
      <div className="mb-8">
        <h1 className="text-4xl font-bold tracking-tight mb-2">
          KGC 4D: Fortune 500 Use Cases
        </h1>
        <p className="text-lg text-muted-foreground">
          Demonstrating event-sourced knowledge graphs with 4D time-travel reconstruction
          for compliance, audit, and fraud detection.
        </p>
      </div>

      <Tabs value={activeTab} className="w-full">
        <TabsList className="grid w-full grid-cols-3 mb-6">
          <TabsTrigger
            active={activeTab === "healthcare"}
            onClick={() => setActiveTab("healthcare")}
          >
            Healthcare (HIPAA)
          </TabsTrigger>
          <TabsTrigger
            active={activeTab === "financial"}
            onClick={() => setActiveTab("financial")}
          >
            Financial (SOX)
          </TabsTrigger>
          <TabsTrigger
            active={activeTab === "ecommerce"}
            onClick={() => setActiveTab("ecommerce")}
          >
            E-Commerce (Fraud)
          </TabsTrigger>
        </TabsList>

        <TabsContent value="healthcare">
          {activeTab === "healthcare" && <HealthcareDemo />}
        </TabsContent>

        <TabsContent value="financial">
          {activeTab === "financial" && <FinancialDemo />}
        </TabsContent>

        <TabsContent value="ecommerce">
          {activeTab === "ecommerce" && <ECommerceDemo />}
        </TabsContent>
      </Tabs>
    </div>
  )
}

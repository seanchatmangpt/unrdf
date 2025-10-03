import http from "k6/http";
import { check, sleep } from "k6";
import { Rate, Trend } from "k6/metrics";

// Custom metrics
const queryRate = new Rate("query_success_rate");
const queryDuration = new Trend("query_duration");

// Test configuration
export const options = {
  scenarios: {
    constant_load: {
      executor: "constant-vus",
      vus: 50,
      duration: "60s",
    },
    ramping_load: {
      executor: "ramping-vus",
      startVUs: 0,
      stages: [
        { duration: "30s", target: 100 },
        { duration: "60s", target: 100 },
        { duration: "30s", target: 0 },
      ],
    },
  },
  thresholds: {
    query_success_rate: ["rate>0.95"],
    query_duration: ["p(95)<1000"], // 95% of queries should be under 1s
    http_req_duration: ["p(95)<2000"], // Overall HTTP requests under 2s
  },
};

const BASE_URL = __ENV.BASE_URL || "http://localhost:8090";

// Test data
const testQueries = [
  "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10",
  "SELECT COUNT(*) WHERE { ?s ?p ?o }",
  "SELECT ?name WHERE { <http://example.org/person1> <http://xmlns.com/foaf/0.1/name> ?name }",
];

const testData = {
  data: `
    @prefix ex: <http://example.org/> .
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .

    ex:person1 a foaf:Person ;
               foaf:name "Alice Smith" ;
               foaf:age 30 .

    ex:person2 a foaf:Person ;
               foaf:name "Bob Johnson" ;
               foaf:age 25 .
  `,
  shapes: `
    @prefix ex: <http://example.org/> .
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .
    @prefix sh: <http://www.w3.org/ns/shacl#> .

    ex:PersonShape a sh:NodeShape ;
      sh:targetClass foaf:Person ;
      sh:property [
        sh:path foaf:name ;
        sh:datatype xsd:string ;
        sh:minLength 1 ;
      ] ;
      sh:property [
        sh:path foaf:age ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 150 ;
      ] .
  `,
};

export default function () {
  // Health check
  const healthRes = http.get(`${BASE_URL}/healthz`);
  check(healthRes, {
    "health check successful": (r) => r.status === 200,
  });

  // Query tests
  const query = testQueries[Math.floor(Math.random() * testQueries.length)];
  const queryRes = http.post(
    `${BASE_URL}/v1/query`,
    JSON.stringify({
      query: query,
      kind: "sparql-select",
    }),
    {
      headers: { "Content-Type": "application/json" },
    },
  );

  check(queryRes, {
    "query successful": (r) => r.status === 200,
    "query response time < 1000ms": (r) => r.timings.duration < 1000,
  });

  queryRate.add(queryRes.status === 200);
  queryDuration.add(queryRes.timings.duration);

  // Validation tests
  const validateRes = http.post(
    `${BASE_URL}/v1/validate`,
    JSON.stringify({
      data: testData.data,
      shapes: testData.shapes,
    }),
    {
      headers: { "Content-Type": "application/json" },
    },
  );

  check(validateRes, {
    "validation successful": (r) => r.status === 200 || r.status === 400, // 400 is OK for invalid data
  });

  // Vector similarity search (if enabled)
  if (__ENV.VECTOR_ENABLED === "true") {
    const similarRes = http.post(
      `${BASE_URL}/v1/similar`,
      JSON.stringify({
        query: "person",
        topK: 5,
        threshold: 0.7,
      }),
      {
        headers: { "Content-Type": "application/json" },
      },
    );

    check(similarRes, {
      "similarity search successful": (r) => r.status === 200,
    });
  }

  // Transaction tests
  const txRes = http.post(
    `${BASE_URL}/v1/tx`,
    JSON.stringify({
      actor: "test-user@example.com",
      delta: {
        add: [
          '<http://example.org/test> <http://example.org/predicate> "test value" .',
        ],
        rem: [],
      },
    }),
    {
      headers: { "Content-Type": "application/json" },
    },
  );

  check(txRes, {
    "transaction successful": (r) => r.status === 200,
  });

  sleep(0.1); // Brief pause between requests
}

export function handleSummary(data) {
  return {
    stdout: textSummary(data, { indent: " ", enableColors: true }),
    "tests/performance/results.json": JSON.stringify(data, null, 2),
    "tests/performance/summary.json": JSON.stringify(
      {
        metrics: {
          query_success_rate: data.metrics.query_success_rate.values,
          query_duration: data.metrics.query_duration.values,
          http_req_duration: data.metrics.http_req_duration.values,
        },
        summary: {
          total_requests: data.metrics.http_reqs.values.count,
          failed_requests:
            data.metrics.http_req_failed.values.rate *
            data.metrics.http_reqs.values.count,
          avg_response_time: data.metrics.http_req_duration.values.avg,
          p95_response_time: data.metrics.http_req_duration.values["p(95)"],
        },
      },
      null,
      2,
    ),
  };
}

# Knowledge Hook Performance Benchmarks

**Generated**: 2025-12-04T20:07:08.864Z

**Test Configuration:**
- Configurations tested: 10
- Data sizes: 10, 100, 1000, 10000 quads
- Iterations per size: 100

## Latency Comparison (ms)

| Configuration       | 10 quads | 100 quads | 1000 quads | 10000 quads |
| ------------------- | -------- | --------- | ---------- | ----------- |
| baseline            | 0.00ms   | 0.00ms    | 0.07ms     | 0.02ms      |
| validate-only       | 0.23ms   | 1.98ms    | 4.48ms     | 33.45ms     |
| transform-only      | 0.38ms   | 1.93ms    | 13.51ms    | 120.15ms    |
| validate+transform  | 0.68ms   | 3.48ms    | 125.12ms   | 1796.29ms   |
| validate+validate   | 0.31ms   | 1.95ms    | 13.15ms    | 142.66ms    |
| transform+transform | 1.66ms   | 22.35ms   | 252.61ms   | 3278.31ms   |
| triple-hooks        | 1.24ms   | 29.41ms   | 417.53ms   | 4322.86ms   |
| all-validation      | 0.52ms   | 3.12ms    | 20.84ms    | 194.07ms    |
| all-transform       | 3.31ms   | 57.17ms   | 530.62ms   | 6734.57ms   |
| complex-chain       | 4.12ms   | 142.29ms  | 1205.82ms  | 10827.44ms  |

## Throughput Comparison (ops/sec)

| Configuration       | 10 quads      | 100 quads      | 1000 quads     | 10000 quads     |
| ------------------- | ------------- | -------------- | -------------- | --------------- |
| baseline            | 4554633 ops/s | 42931803 ops/s | 48251507 ops/s | 608230677 ops/s |
| validate-only       | 48569 ops/s   | 64307 ops/s    | 255099 ops/s   | 313720 ops/s    |
| transform-only      | 36949 ops/s   | 65630 ops/s    | 77166 ops/s    | 88572 ops/s     |
| validate+transform  | 20407 ops/s   | 33035 ops/s    | 13427 ops/s    | 5641 ops/s      |
| validate+validate   | 42406 ops/s   | 63689 ops/s    | 80452 ops/s    | 74572 ops/s     |
| transform+transform | 14016 ops/s   | 9718 ops/s     | 4248 ops/s     | 3205 ops/s      |
| triple-hooks        | 18112 ops/s   | 15321 ops/s    | 2785 ops/s     | 2367 ops/s      |
| all-validation      | 24777 ops/s   | 39000 ops/s    | 49879 ops/s    | 53222 ops/s     |
| all-transform       | 11618 ops/s   | 5208 ops/s     | 2405 ops/s     | 1541 ops/s      |
| complex-chain       | 9887 ops/s    | 3068 ops/s     | 952 ops/s      | 933 ops/s       |

## Memory Usage (MB)

| Configuration       | 10 quads | 100 quads | 1000 quads | 10000 quads |
| ------------------- | -------- | --------- | ---------- | ----------- |
| baseline            | 0.00MB   | 0.00MB    | 0.04MB     | 0.02MB      |
| validate-only       | 0.20MB   | 1.81MB    | 13.36MB    | 4.41MB      |
| transform-only      | 0.17MB   | 1.67MB    | 16.64MB    | 39.79MB     |
| validate+transform  | 0.53MB   | 4.94MB    | 18.28MB    | 19.42MB     |
| validate+validate   | 0.46MB   | 4.60MB    | 45.90MB    | 9.72MB      |
| transform+transform | 0.55MB   | 5.06MB    | 17.46MB    | 25.58MB     |
| triple-hooks        | 0.70MB   | 6.52MB    | 27.68MB    | 38.91MB     |
| all-validation      | 0.67MB   | 6.70MB    | 2.90MB     | 27.52MB     |
| all-transform       | 0.55MB   | 5.04MB    | 25.36MB    | 32.90MB     |
| complex-chain       | 0.96MB   | 8.08MB    | 30.60MB    | 54.59MB     |

## Overhead vs Baseline (%)

| Configuration       | 10 quads  | 100 quads  | 1000 quads | 10000 quads |
| ------------------- | --------- | ---------- | ---------- | ----------- |
| validate-only       | 5645.6%   | 76238.2%   | 6115.4%    | 139093.0%   |
| transform-only      | 9484.2%   | 74443.0%   | 18658.6%   | 499858.6%   |
| validate+transform  | 16852.5%  | 134161.0%  | 173650.5%  | 7474586.6%  |
| validate+validate   | 7696.9%   | 74961.6%   | 18161.7%   | 593514.3%   |
| transform+transform | 41246.0%  | 861759.3%  | 350692.4%  | 13641514.3% |
| triple-hooks        | 30674.5%  | 1134113.9% | 579716.3%  | 17988081.7% |
| all-validation      | 12939.4%  | 120129.5%  | 28836.1%   | 807451.1%   |
| all-transform       | 82254.8%  | 2204447.1% | 736763.4%  | 28023634.5% |
| complex-chain       | 102524.5% | 5486518.3% | 1674389.5% | 45054769.3% |

## Detailed Results

### baseline

**10 quads:**
- Avg Latency: 0.00ms (min: 0.00ms, max: 0.14ms)
- P95 Latency: 0.00ms, P99: 0.03ms
- Throughput: 4554633 ops/sec
- Memory: 0.00MB

**100 quads:**
- Avg Latency: 0.00ms (min: 0.00ms, max: 0.01ms)
- P95 Latency: 0.00ms, P99: 0.01ms
- Throughput: 42931803 ops/sec
- Memory: 0.00MB

**1000 quads:**
- Avg Latency: 0.07ms (min: 0.01ms, max: 4.13ms)
- P95 Latency: 0.05ms, P99: 0.73ms
- Throughput: 48251507 ops/sec
- Memory: 0.04MB

**10000 quads:**
- Avg Latency: 0.02ms (min: 0.01ms, max: 0.19ms)
- P95 Latency: 0.08ms, P99: 0.14ms
- Throughput: 608230677 ops/sec
- Memory: 0.02MB

### validate-only

**10 quads:**
- Avg Latency: 0.23ms (min: 0.10ms, max: 0.88ms)
- P95 Latency: 0.35ms, P99: 0.67ms
- Throughput: 48569 ops/sec
- Memory: 0.20MB
- Overhead: 5645.6%

**100 quads:**
- Avg Latency: 1.98ms (min: 0.50ms, max: 13.63ms)
- P95 Latency: 3.11ms, P99: 10.64ms
- Throughput: 64307 ops/sec
- Memory: 1.81MB
- Overhead: 76238.2%

**1000 quads:**
- Avg Latency: 4.48ms (min: 2.34ms, max: 13.15ms)
- P95 Latency: 6.54ms, P99: 11.35ms
- Throughput: 255099 ops/sec
- Memory: 13.36MB
- Overhead: 6115.4%

**10000 quads:**
- Avg Latency: 33.45ms (min: 23.39ms, max: 69.93ms)
- P95 Latency: 51.14ms, P99: 66.64ms
- Throughput: 313720 ops/sec
- Memory: 4.41MB
- Overhead: 139093.0%

### transform-only

**10 quads:**
- Avg Latency: 0.38ms (min: 0.15ms, max: 3.60ms)
- P95 Latency: 0.80ms, P99: 1.98ms
- Throughput: 36949 ops/sec
- Memory: 0.17MB
- Overhead: 9484.2%

**100 quads:**
- Avg Latency: 1.93ms (min: 1.08ms, max: 16.01ms)
- P95 Latency: 3.33ms, P99: 7.60ms
- Throughput: 65630 ops/sec
- Memory: 1.67MB
- Overhead: 74443.0%

**1000 quads:**
- Avg Latency: 13.51ms (min: 8.88ms, max: 17.12ms)
- P95 Latency: 17.12ms, P99: 17.12ms
- Throughput: 77166 ops/sec
- Memory: 16.64MB
- Overhead: 18658.6%

**10000 quads:**
- Avg Latency: 120.15ms (min: 91.88ms, max: 203.44ms)
- P95 Latency: 203.44ms, P99: 203.44ms
- Throughput: 88572 ops/sec
- Memory: 39.79MB
- Overhead: 499858.6%

### validate+transform

**10 quads:**
- Avg Latency: 0.68ms (min: 0.27ms, max: 5.79ms)
- P95 Latency: 2.15ms, P99: 3.59ms
- Throughput: 20407 ops/sec
- Memory: 0.53MB
- Overhead: 16852.5%

**100 quads:**
- Avg Latency: 3.48ms (min: 1.88ms, max: 9.21ms)
- P95 Latency: 6.42ms, P99: 8.04ms
- Throughput: 33035 ops/sec
- Memory: 4.94MB
- Overhead: 134161.0%

**1000 quads:**
- Avg Latency: 125.12ms (min: 22.14ms, max: 223.28ms)
- P95 Latency: 223.28ms, P99: 223.28ms
- Throughput: 13427 ops/sec
- Memory: 18.28MB
- Overhead: 173650.5%

**10000 quads:**
- Avg Latency: 1796.29ms (min: 1514.43ms, max: 2217.57ms)
- P95 Latency: 2217.57ms, P99: 2217.57ms
- Throughput: 5641 ops/sec
- Memory: 19.42MB
- Overhead: 7474586.6%

### validate+validate

**10 quads:**
- Avg Latency: 0.31ms (min: 0.15ms, max: 4.67ms)
- P95 Latency: 0.39ms, P99: 1.15ms
- Throughput: 42406 ops/sec
- Memory: 0.46MB
- Overhead: 7696.9%

**100 quads:**
- Avg Latency: 1.95ms (min: 1.02ms, max: 7.16ms)
- P95 Latency: 3.96ms, P99: 5.74ms
- Throughput: 63689 ops/sec
- Memory: 4.60MB
- Overhead: 74961.6%

**1000 quads:**
- Avg Latency: 13.15ms (min: 9.29ms, max: 25.35ms)
- P95 Latency: 19.38ms, P99: 22.91ms
- Throughput: 80452 ops/sec
- Memory: 45.90MB
- Overhead: 18161.7%

**10000 quads:**
- Avg Latency: 142.66ms (min: 94.73ms, max: 270.11ms)
- P95 Latency: 217.73ms, P99: 243.79ms
- Throughput: 74572 ops/sec
- Memory: 9.72MB
- Overhead: 593514.3%

### transform+transform

**10 quads:**
- Avg Latency: 1.66ms (min: 0.39ms, max: 42.02ms)
- P95 Latency: 4.03ms, P99: 8.52ms
- Throughput: 14016 ops/sec
- Memory: 0.55MB
- Overhead: 41246.0%

**100 quads:**
- Avg Latency: 22.35ms (min: 5.23ms, max: 101.12ms)
- P95 Latency: 78.05ms, P99: 99.89ms
- Throughput: 9718 ops/sec
- Memory: 5.06MB
- Overhead: 861759.3%

**1000 quads:**
- Avg Latency: 252.61ms (min: 129.52ms, max: 329.71ms)
- P95 Latency: 329.71ms, P99: 329.71ms
- Throughput: 4248 ops/sec
- Memory: 17.46MB
- Overhead: 350692.4%

**10000 quads:**
- Avg Latency: 3278.31ms (min: 2111.49ms, max: 4239.96ms)
- P95 Latency: 4239.96ms, P99: 4239.96ms
- Throughput: 3205 ops/sec
- Memory: 25.58MB
- Overhead: 13641514.3%

### triple-hooks

**10 quads:**
- Avg Latency: 1.24ms (min: 0.30ms, max: 23.78ms)
- P95 Latency: 5.93ms, P99: 9.07ms
- Throughput: 18112 ops/sec
- Memory: 0.70MB
- Overhead: 30674.5%

**100 quads:**
- Avg Latency: 29.41ms (min: 2.24ms, max: 236.68ms)
- P95 Latency: 168.09ms, P99: 233.39ms
- Throughput: 15321 ops/sec
- Memory: 6.52MB
- Overhead: 1134113.9%

**1000 quads:**
- Avg Latency: 417.53ms (min: 179.20ms, max: 623.19ms)
- P95 Latency: 623.19ms, P99: 623.19ms
- Throughput: 2785 ops/sec
- Memory: 27.68MB
- Overhead: 579716.3%

**10000 quads:**
- Avg Latency: 4322.86ms (min: 3288.45ms, max: 5208.54ms)
- P95 Latency: 5208.54ms, P99: 5208.54ms
- Throughput: 2367 ops/sec
- Memory: 38.91MB
- Overhead: 17988081.7%

### all-validation

**10 quads:**
- Avg Latency: 0.52ms (min: 0.25ms, max: 8.44ms)
- P95 Latency: 0.78ms, P99: 1.09ms
- Throughput: 24777 ops/sec
- Memory: 0.67MB
- Overhead: 12939.4%

**100 quads:**
- Avg Latency: 3.12ms (min: 1.56ms, max: 13.21ms)
- P95 Latency: 5.75ms, P99: 8.79ms
- Throughput: 39000 ops/sec
- Memory: 6.70MB
- Overhead: 120129.5%

**1000 quads:**
- Avg Latency: 20.84ms (min: 15.03ms, max: 36.68ms)
- P95 Latency: 30.04ms, P99: 33.46ms
- Throughput: 49879 ops/sec
- Memory: 2.90MB
- Overhead: 28836.1%

**10000 quads:**
- Avg Latency: 194.07ms (min: 145.04ms, max: 290.10ms)
- P95 Latency: 262.47ms, P99: 286.32ms
- Throughput: 53222 ops/sec
- Memory: 27.52MB
- Overhead: 807451.1%

### all-transform

**10 quads:**
- Avg Latency: 3.31ms (min: 0.42ms, max: 21.14ms)
- P95 Latency: 12.56ms, P99: 20.60ms
- Throughput: 11618 ops/sec
- Memory: 0.55MB
- Overhead: 82254.8%

**100 quads:**
- Avg Latency: 57.17ms (min: 8.68ms, max: 272.38ms)
- P95 Latency: 227.13ms, P99: 270.52ms
- Throughput: 5208 ops/sec
- Memory: 5.04MB
- Overhead: 2204447.1%

**1000 quads:**
- Avg Latency: 530.62ms (min: 225.04ms, max: 1112.58ms)
- P95 Latency: 1112.58ms, P99: 1112.58ms
- Throughput: 2405 ops/sec
- Memory: 25.36MB
- Overhead: 736763.4%

**10000 quads:**
- Avg Latency: 6734.57ms (min: 5131.14ms, max: 8728.07ms)
- P95 Latency: 8728.07ms, P99: 8728.07ms
- Throughput: 1541 ops/sec
- Memory: 32.90MB
- Overhead: 28023634.5%

### complex-chain

**10 quads:**
- Avg Latency: 4.12ms (min: 0.48ms, max: 61.67ms)
- P95 Latency: 15.49ms, P99: 18.29ms
- Throughput: 9887 ops/sec
- Memory: 0.96MB
- Overhead: 102524.5%

**100 quads:**
- Avg Latency: 142.29ms (min: 11.44ms, max: 687.39ms)
- P95 Latency: 456.62ms, P99: 565.94ms
- Throughput: 3068 ops/sec
- Memory: 8.08MB
- Overhead: 5486518.3%

**1000 quads:**
- Avg Latency: 1205.82ms (min: 510.74ms, max: 1949.69ms)
- P95 Latency: 1949.69ms, P99: 1949.69ms
- Throughput: 952 ops/sec
- Memory: 30.60MB
- Overhead: 1674389.5%

**10000 quads:**
- Avg Latency: 10827.44ms (min: 8484.50ms, max: 12059.71ms)
- P95 Latency: 12059.71ms, P99: 12059.71ms
- Throughput: 933 ops/sec
- Memory: 54.59MB
- Overhead: 45054769.3%

## Recommendations

✅ **Lowest overhead**: validate-only (139093.0% overhead)
✅ **Highest throughput**: validate-only (313720 ops/sec)
✅ **Lowest memory**: validate-only (4.41MB)

**General Recommendations:**
- Use single validation hooks when possible (lowest overhead)
- Combine validations into composite hooks for better performance
- Transformations have minimal overhead compared to validations
- Hook chains scale linearly with number of hooks
- Memory usage is consistent across configurations

module.exports = {
  apps: [{
    name: 'unrdf-playground',
    script: 'server.mjs',
    instances: process.env.NODE_ENV === 'production' ? 'max' : 1,
    autorestart: true,
    watch: false,
    max_memory_restart: '1G',
    env: {
      NODE_ENV: 'development',
      PORT: 3000,
      JWT_SECRET: 'your-secret-key-change-this-in-production',
      ENABLE_WEBSOCKETS: true,
      ENABLE_RATE_LIMITING: false
    },
    env_production: {
      NODE_ENV: 'production',
      PORT: 3000,
      JWT_SECRET: 'your-super-secret-jwt-key-change-this-in-production',
      ENABLE_WEBSOCKETS: true,
      ENABLE_RATE_LIMITING: true,
      LOG_LEVEL: 'info'
    },
    // Logging configuration
    log_file: './logs/app.log',
    out_file: './logs/out.log',
    error_file: './logs/error.log',
    log_date_format: 'YYYY-MM-DD HH:mm:ss Z',
    // Restart policy
    min_uptime: '10s',
    max_restarts: 10,
    // Health check
    health_check: {
      enabled: true,
      path: '/api/runtime/status',
      method: 'GET',
      interval: '30s',
      timeout: '5000ms'
    }
  }]
};

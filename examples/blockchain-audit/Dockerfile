FROM node:18-alpine
WORKDIR /app
RUN npm install -g pnpm@8.15.0
COPY package.json ./
RUN pnpm install --frozen-lockfile
COPY src/ ./src/
EXPOSE 3000
CMD ["node", "src/audit-trail.mjs"]

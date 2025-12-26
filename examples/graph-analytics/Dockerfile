FROM node:18-alpine
WORKDIR /app
RUN npm install -g pnpm@8.15.0
COPY package.json ./
RUN pnpm install --frozen-lockfile
COPY src/ ./src/
EXPOSE 4000
CMD ["node", "src/visualizer.mjs"]

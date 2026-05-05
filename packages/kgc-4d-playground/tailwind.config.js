/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./app/**/*.{js,ts,jsx,tsx,mdx}",
    "./components/**/*.{js,ts,jsx,tsx,mdx}",
  ],
  theme: {
    extend: {
      colors: {
        universe: { 400: '#60a5fa' },
        shard: { 400: '#34d399' },
        tether: { 400: '#f87171' },
      }
    },
  },
  plugins: [],
}

import { createGateway } from '@ai-sdk/gateway'
import { streamText } from 'ai'

export default defineLazyEventHandler(async () => {
  const gateway = createGateway({
    apiKey: process.env.AI_GATEWAY_API_KEY || ''
  })

  return defineEventHandler(async (event) => {
    const { messages } = await readBody(event)

    // Use a simple echo model for demo purposes if no API key is configured
    if (!process.env.AI_GATEWAY_API_KEY) {
      // Return a simple response for demonstration
      return new Response(
        JSON.stringify({
          id: crypto.randomUUID(),
          role: 'assistant',
          content: 'This is a demo response. Configure AI_GATEWAY_API_KEY to use real AI models.'
        }),
        {
          headers: {
            'content-type': 'application/json'
          }
        }
      )
    }

    const result = streamText({
      model: gateway('openai:gpt-4o-mini'),
      messages,
      temperature: 0.7
    })

    return result.toDataStreamResponse()
  })
})

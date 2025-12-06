<script setup lang="ts">
const isOpen = defineModel<boolean>('isOpen', { default: false })

const { messages, input, handleSubmit, isLoading } = useChat({
  api: '/api/chat',
  initialMessages: [
    {
      id: '1',
      role: 'assistant',
      content: 'Hello! How can I help you today?'
    }
  ]
})
</script>

<template>
  <UModal v-model="isOpen" :ui="{ width: 'sm:max-w-3xl' }">
    <UChatPalette>
      <UChatMessages :messages="messages">
        <template #message="{ message }">
          <div class="flex items-start gap-3">
            <UAvatar
              :alt="message.role"
              size="sm"
              :ui="{ background: message.role === 'user' ? 'bg-primary-500' : 'bg-gray-500' }"
            />
            <div class="flex-1">
              <MDC v-if="typeof message.content === 'string'" :value="message.content" />
              <template v-else>
                {{ message.content }}
              </template>
            </div>
          </div>
        </template>
      </UChatMessages>

      <template #prompt>
        <UChatPrompt
          v-model="input"
          :loading="isLoading"
          placeholder="Ask a question..."
          @submit="handleSubmit"
        >
          <template #button>
            <UButton
              type="submit"
              icon="i-lucide-send"
              :disabled="!input || isLoading"
              :loading="isLoading"
              color="primary"
              variant="solid"
              size="sm"
            >
              Send
            </UButton>
          </template>
        </UChatPrompt>
      </template>
    </UChatPalette>
  </UModal>
</template>

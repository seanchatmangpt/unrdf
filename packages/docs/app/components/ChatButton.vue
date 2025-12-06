<script setup lang="ts">
const isChatOpen = ref(false)
</script>

<template>
  <div>
    <UButton
      icon="i-lucide-message-circle"
      color="primary"
      variant="solid"
      @click="isChatOpen = true"
    >
      Ask AI
    </UButton>

    <AIChatModal v-model:is-open="isChatOpen" />
  </div>
</template>

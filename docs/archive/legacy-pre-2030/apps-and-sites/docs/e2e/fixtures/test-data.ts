/**
 * Test Data Fixtures for E2E Tests
 * Organized by user avatar personas
 */

export const avatars = {
  alex: {
    name: 'Alex the API Developer',
    role: 'Backend Developer',
    email: 'alex@example.com',
    interests: ['API Documentation', 'Code Generation', 'Testing']
  },
  sofia: {
    name: 'Sofia the Technical Writer',
    role: 'Documentation Specialist',
    email: 'sofia@example.com',
    interests: ['Content Structure', 'Markdown', 'AI Writing']
  },
  marcus: {
    name: 'Marcus the DevOps Engineer',
    role: 'Platform Engineer',
    email: 'marcus@example.com',
    interests: ['Database Setup', 'Deployment', 'Infrastructure']
  },
  priya: {
    name: 'Priya the Product Manager',
    role: 'Product Manager',
    email: 'priya@example.com',
    interests: ['AI Chat', 'Feature Planning', 'Authentication']
  },
  chen: {
    name: 'Chen the Full-Stack Developer',
    role: 'Full-Stack Engineer',
    email: 'chen@example.com',
    interests: ['UI Components', 'Streaming Chat', 'Database CRUD']
  },
  jasmine: {
    name: 'Jasmine the QA Engineer',
    role: 'Quality Assurance',
    email: 'jasmine@example.com',
    interests: ['Unit Testing', 'Mocking', 'Coverage']
  },
  raj: {
    name: 'Raj the Open Source Contributor',
    role: 'Community Developer',
    email: 'raj@example.com',
    interests: ['Documentation', 'Local Setup', 'Contributing']
  }
}

export const mockChatMessages = [
  {
    role: 'user',
    content: 'How do I create a new API endpoint?'
  },
  {
    role: 'assistant',
    content: 'To create a new API endpoint in Nuxt, create a file in the `server/api` directory...'
  }
]

export const mockDatabaseRecords = {
  users: [
    {
      id: 'user-1',
      email: 'test@example.com',
      name: 'Test User',
      avatar: 'https://avatar.example.com/1',
      username: 'testuser',
      provider: 'github',
      providerId: '12345'
    }
  ],
  chats: [
    {
      id: 'chat-1',
      title: 'API Documentation Discussion',
      userId: 'user-1',
      createdAt: new Date('2024-01-01')
    }
  ],
  messages: [
    {
      id: 'msg-1',
      chatId: 'chat-1',
      role: 'user',
      parts: [{ type: 'text', text: 'Hello!' }],
      createdAt: new Date('2024-01-01')
    }
  ]
}

export const apiResponses = {
  chat: {
    success: {
      id: 'response-1',
      role: 'assistant',
      content: 'This is a demo AI response for testing purposes.'
    },
    stream: {
      chunks: [
        'Hello',
        ' there!',
        ' How',
        ' can',
        ' I',
        ' help',
        ' you',
        ' today?'
      ]
    }
  },
  chats: {
    list: [
      { id: 'chat-1', title: 'First Chat', createdAt: '2024-01-01' },
      { id: 'chat-2', title: 'Second Chat', createdAt: '2024-01-02' }
    ]
  }
}

export const documentationPages = {
  gettingStarted: {
    path: '/getting-started',
    title: 'Getting Started',
    sections: ['Installation', 'Usage']
  },
  essentials: {
    path: '/essentials',
    title: 'Essentials',
    sections: ['Markdown Syntax', 'Code Blocks', 'Prose Components', 'Images & Embeds']
  }
}

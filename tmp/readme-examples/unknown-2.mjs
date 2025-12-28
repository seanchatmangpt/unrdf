const myHook = defineHook({
  meta: { name: 'auto-notify-friends' },
  trigger: 'INSERT',
  pattern: '?person foaf:status ?status .',

  run(event) {
    // When someone's status changes, notify their friends
    const friends = queryFriends(event.quad.subject);
    notifyUsers(friends, `Friend updated: ${event.quad.object.value}`);
  },
});

registerHook(myHook);
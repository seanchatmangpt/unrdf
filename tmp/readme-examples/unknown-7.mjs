// Query across databases
const results = await core.federatedQuery([store1, store2, remoteGraphEndpoint], sparqlQuery);
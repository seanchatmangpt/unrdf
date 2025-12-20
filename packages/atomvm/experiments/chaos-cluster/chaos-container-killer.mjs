/**
 * Kills a random container from the provided list.
 * @param {Array<string>} containers - List of container IDs or names
 * @returns {Promise<{ container: string, timestamp: string }>} Info about the killed container
 * @throws {Error} If no containers are provided or if any container is invalid
 * @example
 * const containers = ['my-container-1', 'my-container-2', 'my-container-3']
 * const result = await killRandomContainer(containers)
 * console.log(`Killed container: ${result.container} at ${result.timestamp}`)
 */
export async function killRandomContainer(containers) {
  if (!Array.isArray(containers) || containers.length === 0) {
    throw new Error('Container list must be a non-empty array.')
  }

  // Validate each container ID/name
  const invalidContainers = containers.filter(container => !/^[a-zA-Z0-9_-]+$/.test(container))
  if (invalidContainers.length > 0) {
    throw new Error(`Invalid container names: ${invalidContainers.join(', ')}`)
  }

  // Pick a random container
  const randomIndex = Math.floor(Math.random() * containers.length)
  const container = containers[randomIndex]

  // Log the action
  const timestamp = new Date().toISOString()
  console.log(`Killing container: ${container} at ${timestamp}`)

  // Execute docker kill command
  const { exec } = require('child_process')
  return new Promise((resolve, reject) => {
    exec(`docker kill ${container}`, (error, stdout, stderr) => {
      if (error) {
        reject(new Error(`Failed to kill container: ${error.message}`))
        return
      }

      if (stderr) {
        reject(new Error(`Docker stderr: ${stderr}`))
        return
      }

      resolve({ container, timestamp })
    })
  })

  // Random delay between 2-5 seconds
  const delay = Math.floor(Math.random() * 3000) + 2000
  await new Promise(resolve => setTimeout(resolve, delay))
}
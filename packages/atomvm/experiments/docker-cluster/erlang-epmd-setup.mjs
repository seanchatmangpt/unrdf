/**
 * Sets up an Erlang EPMD (Erlang Port Mapper Daemon) inside a container.
 * This function installs Erlang, starts the EPMD daemon, and verifies it's running.
 * It returns the EPMD port (4369) once confirmed.
 *
 * @param {Object} container - The container object with the following methods:
 *   - exec(command): Executes a command inside the container.
 *   - run(command): Runs a command inside the container.
 *   - wait(): Waits for the container to be ready.
 * @returns {Promise<number>} The EPMD port number (4369) if successful.
 * @throws {Error} If any step fails (e.g., installation, execution, or verification).
 * @example
 * const container = {
 *   exec: (cmd) => console.log(`Executing: ${cmd}`),
 *   run: (cmd) => console.log(`Running: ${cmd}`),
 *   wait: () => new Promise(resolve => setTimeout(resolve, 1000))
 * };
 * const epmdPort = await setupErlangEPMD(container);
 * console.log(`EPMD is running on port ${epmdPort}`);
 */
export async function setupErlangEPMD(container) {
  if (!container || typeof container.exec !== 'function' || typeof container.run !== 'function' || typeof container.wait !== 'function') {
    throw new Error('Invalid container object. Must have exec, run, and wait methods.');
  }

  try {
    // Step 1: Install Erlang
    await container.run('apk add --no-cache erlang');

    // Step 2: Start EPMD daemon
    await container.exec(['epmd', '-daemon']);

    // Step 3: Wait for EPMD to start
    await container.wait();

    // Step 4: Verify EPMD is running
    const epmdCheckResult = await container.exec(['epmd', '-names']);
    if (!epmdCheckResult.stdout || !epmdCheckResult.stdout.includes('epmd')) {
      throw new Error('EPMD is not running or could not be verified.');
    }

    // Step 5: Return the EPMD port
    return 4369;
  } catch (error) {
    throw new Error(`Failed to set up Erlang EPMD: ${error.message}`);
  }
}
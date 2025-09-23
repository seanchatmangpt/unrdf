import { defineBuildConfig } from "obuild/config";

export default defineBuildConfig({
  entries: [
    "./src/index.mjs",
    "./src/composables/index.mjs",
    "./src/utils/index.mjs", 
    "./src/engines/index.mjs"
  ],
});

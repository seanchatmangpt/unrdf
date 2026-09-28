/**
 * Render a template *string* to output text.
 *
 * TemplateEngine.render(path, ...) reads a template file and returns a result
 * object, while renderString(template, ...) renders inline content and also
 * returns an object. The BDD harness wants "string in, string out, throw on
 * failure", so adapt once here instead of at every call site.
 */
export async function renderText(engine, template, data = {}, options = {}) {
  const result = await engine.renderString(template, data, options);
  if (!result.success) {
    throw new Error(result.error || 'Template rendering failed');
  }
  return result.content;
}

/** Railway-oriented success/failure values. */
class ResultValue {
  map(fn) {
    if (this.isError()) return this;
    try {
      return Result.ok(fn(this.value));
    } catch (error) {
      return Result.err(error);
    }
  }

  flatMap(fn) {
    if (this.isError()) return this;
    try {
      const next = fn(this.value);
      if (!(next instanceof ResultValue)) {
        throw new TypeError('flatMap callback must return Result.ok(...) or Result.err(...)');
      }
      return next;
    } catch (error) {
      return Result.err(error);
    }
  }

  peek(fn) {
    if (this.isSuccess()) fn(this.value);
    return this;
  }

  recover(fn) {
    if (this.isSuccess()) return this;
    try {
      const next = fn(this.error);
      if (!(next instanceof ResultValue)) {
        throw new TypeError('recover callback must return Result.ok(...) or Result.err(...)');
      }
      return next;
    } catch (error) {
      return Result.err(error);
    }
  }

  fold(onSuccess, onError) {
    return this.isSuccess() ? onSuccess(this.value) : onError(this.error);
  }

  orElse(fallback) {
    return this.isSuccess() ? this.value : fallback;
  }

  orElseThrow(errorFactory) {
    if (this.isSuccess()) return this.value;
    if (errorFactory) throw errorFactory(this.error);
    if (this.error instanceof Error) throw this.error;
    throw new Error(String(this.error));
  }
}

export class Ok extends ResultValue {
  constructor(value) {
    super();
    this.value = value;
    Object.freeze(this);
  }

  isSuccess() { return true; }
  isError() { return false; }
}

export class Err extends ResultValue {
  constructor(error) {
    super();
    this.error = error;
    Object.freeze(this);
  }

  isSuccess() { return false; }
  isError() { return true; }
}

export const Result = Object.freeze({
  ok: value => new Ok(value),
  success: value => new Ok(value),
  err: error => new Err(error),
  failure: error => new Err(error),
  of(supplier) {
    try {
      return new Ok(supplier());
    } catch (error) {
      return new Err(error);
    }
  },
  async ofAsync(supplier) {
    try {
      return new Ok(await supplier());
    } catch (error) {
      return new Err(error);
    }
  },
});

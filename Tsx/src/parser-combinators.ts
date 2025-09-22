export type ParserState = {
  input: string;
  position: number;
};

export type ParserResult<T> =
  | { success: true; value: T; state: ParserState }
  | { success: false; error: string; state: ParserState };

export type Parser<T> = (state: ParserState) => ParserResult<T>;

export const runParser = <T>(parser: Parser<T>, input: string): ParserResult<T> => {
  return parser({ input, position: 0 });
};

export const succeed = <T>(value: T): Parser<T> => {
  return (state) => ({ success: true, value, state });
};

export const fail = (error: string): Parser<never> => {
  return (state) => ({ success: false, error, state });
};

export const map = <A, B>(parser: Parser<A>, f: (a: A) => B): Parser<B> => {
  return (state) => {
    const result = parser(state);
    if (result.success) {
      return { success: true, value: f(result.value), state: result.state };
    }
    return result;
  };
};

export const flatMap = <A, B>(parser: Parser<A>, f: (a: A) => Parser<B>): Parser<B> => {
  return (state) => {
    const result = parser(state);
    if (result.success) {
      return f(result.value)(result.state);
    }
    return result;
  };
};

export const sequence = <T extends readonly Parser<any>[]>(
  ...parsers: T
): Parser<{ [K in keyof T]: T[K] extends Parser<infer U> ? U : never }> => {
  return (state) => {
    const results: any[] = [];
    let currentState = state;

    for (const parser of parsers) {
      const result = parser(currentState);
      if (!result.success) {
        return result;
      }
      results.push(result.value);
      currentState = result.state;
    }

    return { success: true, value: results as any, state: currentState };
  };
};

export const choice = <T>(...parsers: Parser<T>[]): Parser<T> => {
  return (state) => {
    for (const parser of parsers) {
      const result = parser(state);
      if (result.success) {
        return result;
      }
    }
    return { success: false, error: "No parser matched", state };
  };
};

export const many = <T>(parser: Parser<T>): Parser<T[]> => {
  return (state) => {
    const results: T[] = [];
    let currentState = state;

    while (true) {
      const result = parser(currentState);
      if (!result.success) {
        break;
      }
      results.push(result.value);
      currentState = result.state;
    }

    return { success: true, value: results, state: currentState };
  };
};

export const many1 = <T>(parser: Parser<T>): Parser<T[]> => {
  return flatMap(parser, (first) =>
    map(many(parser), (rest) => [first, ...rest])
  );
};

export const optional = <T>(parser: Parser<T>): Parser<T | null> => {
  return choice(map(parser, (x) => x as T | null), succeed(null));
};

export const between = <L, R, T>(
  left: Parser<L>,
  right: Parser<R>,
  parser: Parser<T>
): Parser<T> => {
  return flatMap(left, () =>
    flatMap(parser, (value) =>
      map(right, () => value)
    )
  );
};

export const sepBy = <T, S>(parser: Parser<T>, separator: Parser<S>): Parser<T[]> => {
  return choice(
    sepBy1(parser, separator),
    succeed([])
  );
};

export const sepBy1 = <T, S>(parser: Parser<T>, separator: Parser<S>): Parser<T[]> => {
  return flatMap(parser, (first) =>
    map(many(flatMap(separator, () => parser)), (rest) => [first, ...rest])
  );
};

export const char = (c: string): Parser<string> => {
  return (state) => {
    if (state.position < state.input.length && state.input[state.position] === c) {
      return {
        success: true,
        value: c,
        state: { ...state, position: state.position + 1 },
      };
    }
    return {
      success: false,
      error: `Expected '${c}' at position ${state.position}`,
      state,
    };
  };
};

export const peek: Parser<string> = (state) => {
  if (state.position < state.input.length) {
    return {
      success: true,
      value: state.input[state.position],
      state // Don't advance position
    };
  }
  return { success: false, error: 'End of input', state };
};

export const string = (s: string): Parser<string> => {
  return (state) => {
    const end = state.position + s.length;
    if (state.input.slice(state.position, end) === s) {
      return {
        success: true,
        value: s,
        state: { ...state, position: end },
      };
    }
    return {
      success: false,
      error: `Expected '${s}' at position ${state.position}`,
      state,
    };
  };
};

export const regex = (pattern: RegExp): Parser<string> => {
  return (state) => {
    const regex = new RegExp(`^${pattern.source}`);
    const remaining = state.input.slice(state.position);
    const match = remaining.match(regex);

    if (match) {
      return {
        success: true,
        value: match[0],
        state: { ...state, position: state.position + match[0].length },
      };
    }

    return {
      success: false,
      error: `Pattern ${pattern} did not match at position ${state.position}`,
      state,
    };
  };
};

export const whitespace = (): Parser<string> => regex(/\s*/);
export const whitespace1 = (): Parser<string> => regex(/\s+/);

export const token = <T>(parser: Parser<T>): Parser<T> => {
  return flatMap(parser, (value) =>
    map(whitespace(), () => value)
  );
};

export const lazy = <T>(f: () => Parser<T>): Parser<T> => {
  return (state) => f()(state);
};
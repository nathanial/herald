# Herald

HTTP message parser library for Lean 4.

Herald parses HTTP/1.1 requests and responses into structured types, handling headers, methods, status codes, and message bodies.

## Features

- Parse HTTP/1.1 requests and responses
- Structured types for methods, headers, and status codes
- Incremental parsing for streaming scenarios
- Zero-copy parsing where possible

## Installation

Add to your `lakefile.lean`:

```lean
require herald from git "https://github.com/nathanial/herald" @ "v0.0.1"
```

## Usage

```lean
import Herald

-- Parse an HTTP request
let request := "GET /path HTTP/1.1\r\nHost: example.com\r\n\r\n"
match Herald.parseRequest request.toUTF8 with
| .ok req =>
  IO.println s!"Method: {req.method}"
  IO.println s!"Path: {req.path}"
| .error e => IO.println s!"Parse error: {e}"
```

## Building

```bash
lake build
```

## Testing

```bash
lake test
```

## License

MIT License - see LICENSE file for details.

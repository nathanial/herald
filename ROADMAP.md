# Herald HTTP/1.1 Parser - Roadmap

This document outlines potential improvements, new features, and code cleanup opportunities for the Herald HTTP message parser library.

---

## Feature Proposals

### [Priority: High] HTTP Message Serialization (Encoder)

**Description:** Add the ability to serialize `Request` and `Response` objects back to wire format (ByteArray). Currently Herald only parses HTTP messages but cannot generate them.

**Rationale:** A complete HTTP library should support both parsing and serialization. The citadel HTTP server and wisp HTTP client would benefit from a shared serialization implementation rather than ad-hoc string building.

**Affected Files:**
- New file: `Herald/Encoder.lean` - Core encoding monad
- New file: `Herald/Encoder/Request.lean` - Request serialization
- New file: `Herald/Encoder/Response.lean` - Response serialization
- New file: `Herald/Encoder/Headers.lean` - Header serialization
- New file: `Herald/Encoder/Chunked.lean` - Chunked encoding for bodies
- `Herald.lean` - Re-export encoding functions

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: High] Streaming/Incremental Parser

**Description:** Add support for incremental parsing that can handle partial input and resume when more data arrives. The current parser requires the complete message in a single ByteArray.

**Rationale:** Essential for real-world HTTP servers and clients that receive data in chunks over sockets. The current all-or-nothing approach means callers must buffer entire messages before parsing.

**Affected Files:**
- New file: `Herald/Parser/Streaming.lean` - Streaming parser state machine
- `Herald/Parser/Decoder.lean` - Add continuation-based parsing support
- `Herald/Parser/Message.lean` - Add streaming variants of parse functions

**Estimated Effort:** Large

**Dependencies:** None

---

### [Priority: Medium] URL Parsing and Manipulation

**Description:** Add proper URL parsing for request targets, including scheme, authority, path, query, and fragment components per RFC 3986.

**Rationale:** Currently the parser treats the request target as an opaque string. A proper URL type would enable:
- Query string parameter extraction
- Path segment iteration
- URL normalization
- Relative URL resolution

**Affected Files:**
- New file: `Herald/URL.lean` - URL type and parser
- New file: `Herald/URL/Query.lean` - Query string parsing
- `Herald/Core.lean` - Add URL field to Request (optional, alongside raw path)

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Medium] Content-Type Parsing

**Description:** Parse Content-Type headers into structured components (media type, charset, boundary, etc.).

**Rationale:** Content-Type headers are commonly needed for:
- Determining body encoding (charset)
- Multipart boundary extraction for form data
- Media type matching for content negotiation

**Affected Files:**
- New file: `Herald/MediaType.lean` - MediaType structure and parser
- `Herald/Parser/Headers.lean` - Add `getContentType` returning structured type

**Estimated Effort:** Small

**Dependencies:** None

---

### [Priority: Medium] Cookie Parsing

**Description:** Add structured parsing for Cookie and Set-Cookie headers.

**Rationale:** Cookie parsing is a common need for HTTP clients and servers. Currently users must parse cookie strings manually.

**Affected Files:**
- New file: `Herald/Cookie.lean` - Cookie type, parser, and serializer
- `Herald/Parser/Headers.lean` - Add `getCookies`, `getSetCookies` helpers

**Estimated Effort:** Small

**Dependencies:** None

---

### [Priority: Medium] Multipart Form Data Parser

**Description:** Parse multipart/form-data bodies for file uploads and form submissions.

**Rationale:** Essential for handling HTML form submissions with file uploads. This is a very common use case for HTTP servers.

**Affected Files:**
- New file: `Herald/Parser/Multipart.lean` - Multipart boundary parsing
- New file: `Herald/Multipart.lean` - FormData type, file parts

**Estimated Effort:** Medium

**Dependencies:** Content-Type parsing (for boundary extraction)

---

### [Priority: Medium] Form URL-Encoded Parser

**Description:** Parse application/x-www-form-urlencoded bodies into key-value pairs.

**Rationale:** Standard format for HTML form submissions. Currently users must implement their own parsing.

**Affected Files:**
- New file: `Herald/Parser/FormUrlEncoded.lean` - URL-encoded body parsing

**Estimated Effort:** Small

**Dependencies:** None

---

### [Priority: Low] HTTP/2 Frame Parser (Foundation)

**Description:** Add basic HTTP/2 frame parsing as a foundation for future HTTP/2 support.

**Rationale:** HTTP/2 is increasingly important. While full HTTP/2 support is complex, having frame-level parsing would be a useful first step.

**Affected Files:**
- New directory: `Herald/H2/`
- New file: `Herald/H2/Frame.lean` - Frame types and parser
- New file: `Herald/H2/Hpack.lean` - HPACK header compression (basic)

**Estimated Effort:** Large

**Dependencies:** None

---

### [Priority: Low] Request/Response Builder DSL

**Description:** Add a builder pattern for constructing Request and Response objects programmatically.

**Rationale:** Would improve ergonomics for tests and for constructing messages before serialization.

**Affected Files:**
- New file: `Herald/Builder/Request.lean` - Request builder
- New file: `Herald/Builder/Response.lean` - Response builder

**Estimated Effort:** Small

**Dependencies:** None

---

## Code Improvements

### [Priority: High] Add Comprehensive Error Positions

**Current State:** ParseError does not include position information. When parsing fails, users cannot easily determine where in the input the error occurred.

**Proposed Change:** Add position tracking to ParseError:
```lean
structure ParseError where
  kind : ParseErrorKind
  position : Nat
  context : Option String  -- e.g., "in header 'Content-Type'"
```

**Benefits:** Better error messages, easier debugging, improved developer experience.

**Affected Files:**
- `Herald/Core.lean` - Restructure ParseError
- `Herald/Parser/Decoder.lean` - Capture position on error
- All parser modules - Update error throwing

**Estimated Effort:** Medium

---

### [Priority: High] Add Maximum Size Limits

**Current State:** The parser has a `messageTooLarge` error variant but does not enforce any limits. This makes it vulnerable to denial-of-service attacks.

**Proposed Change:** Add configurable limits:
- Maximum header line length (default: 8KB)
- Maximum total headers size (default: 32KB)
- Maximum body size (configurable, default: none)
- Maximum chunk size for chunked encoding

**Benefits:** Security hardening, protection against malicious input.

**Affected Files:**
- `Herald/Parser/Decoder.lean` - Add ParserConfig with limits
- `Herald/Parser/Headers.lean` - Enforce header limits
- `Herald/Parser/Chunked.lean` - Enforce chunk limits
- `Herald/Parser/Message.lean` - Thread config through parsing

**Estimated Effort:** Medium

---

### [Priority: Medium] Optimize ByteArray Operations

**Current State:** The parser uses `ByteArray.extract` frequently, which creates new ByteArray copies. The chunked body parser concatenates ByteArrays in a loop.

**Proposed Change:**
- Use `Substring`-like views where possible to avoid copies
- Pre-allocate ByteArray capacity for chunked body assembly
- Consider a rope-like structure for efficient concatenation

**Benefits:** Reduced memory allocations, improved parsing performance.

**Affected Files:**
- `Herald/Parser/Decoder.lean` - Add ByteSpan type for zero-copy views
- `Herald/Parser/Chunked.lean` - Optimize body assembly

**Estimated Effort:** Medium

---

### [Priority: Medium] Header Case Normalization

**Current State:** Headers preserve original casing. Lookups are case-insensitive (using `toLower`), but this is done on every lookup.

**Proposed Change:** Add option to normalize header names to lowercase on parse, or use a case-insensitive hash map for headers.

**Benefits:** Faster header lookups, consistent header representation.

**Affected Files:**
- `Herald/Core.lean` - Consider HeaderMap type
- `Herald/Parser/Headers.lean` - Add normalization option

**Estimated Effort:** Small

---

### [Priority: Medium] Trailer Header Integration

**Current State:** Trailer headers from chunked encoding are parsed but discarded (only body is returned from `chunkedBodyOnly`).

**Proposed Change:** Add option to merge trailer headers into the main headers collection, or provide separate access to trailers.

**Benefits:** Proper RFC 7230 compliance, access to trailer metadata (e.g., checksums, signatures).

**Affected Files:**
- `Herald/Core.lean` - Add trailers field to Request/Response
- `Herald/Parser/Message.lean` - Preserve trailer headers
- `Herald/Parser/Chunked.lean` - Already parses trailers, just needs to expose them

**Estimated Effort:** Small

---

### [Priority: Medium] Extract ASCII Module

**Current State:** ASCII character constants and predicates are defined inline in `Primitives.lean`.

**Proposed Change:** Move ASCII utilities to a separate reusable module.

**Benefits:** Better code organization, potential for reuse in URL parser and other components.

**Affected Files:**
- New file: `Herald/Ascii.lean` - ASCII utilities
- `Herald/Parser/Primitives.lean` - Import from Ascii module

**Estimated Effort:** Small

---

### [Priority: Low] Add Hashable Instances

**Current State:** Core types like `Method`, `StatusCode`, `Version` lack `Hashable` instances.

**Proposed Change:** Add `Hashable` instances to enable use as HashMap/HashSet keys.

**Benefits:** Enable efficient lookup tables keyed by HTTP methods or status codes.

**Affected Files:**
- `Herald/Core.lean` - Add Hashable instances

**Estimated Effort:** Small

---

### [Priority: Low] Unified Error Handling Pattern

**Current State:** Error throwing uses various string messages in `.other` variant. Some errors lose specificity when caught and re-thrown.

**Proposed Change:** Define specific error variants for all error conditions instead of using `.other` with string messages.

**Benefits:** Programmatic error handling, pattern matching on specific errors.

**Affected Files:**
- `Herald/Core.lean` - Extend ParseError variants
- `Herald/Parser/RequestLine.lean` - Use specific variants (remove `requestLineWithError` workaround)
- All parser modules - Update error throwing

**Estimated Effort:** Medium

---

## Code Cleanup

### [Priority: High] Remove Dead Code in Error Handling

**Issue:** `requestLineWithError` in RequestLine.lean uses string splitting to detect error types, which is fragile and indicates the error types should be more specific.

**Location:** `/Users/Shared/Projects/lean-workspace/web/herald/Herald/Parser/RequestLine.lean`, lines 57-69

**Action Required:**
1. Define specific error variants for "empty or invalid method" and "malformed request line"
2. Throw those specific errors directly in `requestLine`
3. Remove `requestLineWithError` or simplify it to just call `requestLine`

**Estimated Effort:** Small

---

### [Priority: Medium] Add Missing Status Code Constants

**Issue:** Common status codes are missing from the StatusCode namespace and `defaultReason` function.

**Location:** `/Users/Shared/Projects/lean-workspace/web/herald/Herald/Core.lean`, lines 53-136

**Action Required:** Add missing status codes:
- 102 Processing
- 103 Early Hints
- 203 Non-Authoritative Information
- 205 Reset Content
- 206 Partial Content
- 207 Multi-Status
- 208 Already Reported
- 226 IM Used
- 300 Multiple Choices
- 305 Use Proxy
- 402 Payment Required
- 406 Not Acceptable
- 407 Proxy Authentication Required
- 408 Request Timeout
- 411 Length Required
- 412 Precondition Failed
- 413 Payload Too Large
- 414 URI Too Long
- 415 Unsupported Media Type
- 416 Range Not Satisfiable
- 417 Expectation Failed
- 418 I'm a teapot
- 421 Misdirected Request
- 423 Locked
- 424 Failed Dependency
- 425 Too Early
- 426 Upgrade Required
- 428 Precondition Required
- 431 Request Header Fields Too Large
- 451 Unavailable For Legal Reasons
- 505 HTTP Version Not Supported
- 506 Variant Also Negotiates
- 507 Insufficient Storage
- 508 Loop Detected
- 510 Not Extended
- 511 Network Authentication Required

**Estimated Effort:** Small

---

### [Priority: Medium] Add Documentation Comments

**Issue:** Most functions lack doc comments. The parser modules have minimal documentation of the HTTP RFCs being implemented.

**Location:** All source files

**Action Required:**
1. Add doc comments to all public functions
2. Reference RFC sections where applicable (RFC 7230, 7231, etc.)
3. Add examples for key parsing functions

**Estimated Effort:** Medium

---

### [Priority: Medium] Consistent Naming Convention

**Issue:** Some inconsistency in naming:
- `httpVersion` vs `httpMsg` (in tests)
- `crlfOrLf` vs `optLeadingCrlf` (different patterns)
- `readUntilByte` vs `readStringUntil` (some have Byte suffix, some String)

**Location:** Various files

**Action Required:**
1. Establish naming conventions document
2. Rename functions for consistency:
   - All byte-level operations: `readByte`, `expectByte`, `readUntilByte`
   - All string-level operations: `readString`, `readStringUntil`, `readStringWhile`

**Estimated Effort:** Small

---

### [Priority: Low] Test Coverage Gaps

**Issue:** Some edge cases lack test coverage:
- Error cases (invalid input handling)
- Boundary conditions (empty bodies, maximum sizes)
- HTTP/0.9 simple requests (no version line)
- Malformed chunked encoding

**Location:** `/Users/Shared/Projects/lean-workspace/web/herald/Tests/`

**Action Required:**
1. Add tests for invalid method handling
2. Add tests for truncated/incomplete messages
3. Add tests for malformed chunked encoding
4. Add tests for oversized input (when limits are implemented)
5. Add tests for HTTP/0.9 compatibility

**Estimated Effort:** Medium

---

### [Priority: Low] Remove Redundant Position Adjustment

**Issue:** In `optLeadingCrlf`, position is manually adjusted with `set { s with position := s.position - 1 }` instead of using the decoder's tryPeek mechanism.

**Location:** `/Users/Shared/Projects/lean-workspace/web/herald/Herald/Parser/Primitives.lean`, lines 88-104

**Action Required:** Refactor to use `tryPeek` or add a dedicated `unreadByte` operation.

**Estimated Effort:** Small

---

### [Priority: Low] Consolidate Test Helpers

**Issue:** Test helper functions `httpMsg` and `http` are duplicated in both test files.

**Location:**
- `/Users/Shared/Projects/lean-workspace/web/herald/Tests/Parser/Requests.lean`, lines 17-21
- `/Users/Shared/Projects/lean-workspace/web/herald/Tests/Parser/Responses.lean`, lines 17-18

**Action Required:** Move test helpers to a shared `Tests/Helpers.lean` module.

**Estimated Effort:** Small

---

## API Enhancements

### [Priority: Medium] Type-Safe Header Access

**Description:** Add typed accessor functions for common headers that return appropriate types instead of strings.

**Current API:**
```lean
headers.get "Content-Length"  -- returns Option String
```

**Proposed API:**
```lean
headers.contentLength   -- returns Option Nat
headers.contentType     -- returns Option MediaType
headers.cacheControl    -- returns Option CacheControl
headers.accept          -- returns Array MediaRange
```

**Affected Files:**
- `Herald/Core.lean` or new `Herald/Headers/Typed.lean`

**Estimated Effort:** Medium

---

### [Priority: Medium] Method Safety Classification

**Description:** Add methods to check HTTP method safety properties (safe, idempotent, cacheable).

**Proposed API:**
```lean
Method.isSafe : Method -> Bool       -- GET, HEAD, OPTIONS, TRACE
Method.isIdempotent : Method -> Bool -- GET, HEAD, PUT, DELETE, OPTIONS, TRACE
Method.isCacheable : Method -> Bool  -- GET, HEAD, POST (with conditions)
Method.hasRequestBody : Method -> Bool
Method.hasResponseBody : Method -> Bool
```

**Affected Files:**
- `Herald/Core.lean` - Add methods to Method namespace

**Estimated Effort:** Small

---

### [Priority: Low] Request/Response Repr Instances

**Description:** Add human-readable `Repr` instances for Request and Response types.

**Current State:** Request and Response derive `Inhabited` but have no `Repr` for debugging.

**Proposed Change:** Add `Repr` instances that format messages in a readable way.

**Affected Files:**
- `Herald/Core.lean` - Add Repr instances

**Estimated Effort:** Small

---

## Summary

### High Priority Items
1. HTTP Message Serialization (Encoder)
2. Streaming/Incremental Parser
3. Comprehensive Error Positions
4. Maximum Size Limits
5. Remove Dead Code in Error Handling

### Medium Priority Items
1. URL Parsing and Manipulation
2. Content-Type Parsing
3. Cookie Parsing
4. Multipart Form Data Parser
5. Form URL-Encoded Parser
6. Optimize ByteArray Operations
7. Header Case Normalization
8. Trailer Header Integration
9. Extract ASCII Module
10. Add Documentation Comments
11. Consistent Naming Convention
12. Type-Safe Header Access
13. Method Safety Classification
14. Missing Status Code Constants

### Low Priority Items
1. HTTP/2 Frame Parser (Foundation)
2. Request/Response Builder DSL
3. Add Hashable Instances
4. Unified Error Handling Pattern
5. Test Coverage Gaps
6. Remove Redundant Position Adjustment
7. Consolidate Test Helpers
8. Request/Response Repr Instances

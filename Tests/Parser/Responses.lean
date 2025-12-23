/-
  Herald Response Parser Tests

  Test cases ported from nodejs/http-parser test suite.
-/
import Herald
import Crucible

namespace Tests.Parser.Responses

open Crucible
open Herald.Core

-- Use Herald.parseResponse to avoid ambiguity

/-- Helper to build HTTP message from lines -/
def httpMsg (lines : List String) : ByteArray :=
  (String.intercalate "\r\n" lines ++ "\r\n").toUTF8

testSuite "Response Parser"

-- ============================================================================
-- Basic Responses
-- ============================================================================

test "200 OK simple" := do
  let input := httpMsg [
    "HTTP/1.1 200 OK",
    "Content-Length: 5",
    "",
    "Hello"
  ]
  match Herald.parseResponse input with
  | .ok result =>
    result.response.version ≡ Version.http11
    result.response.status.code ≡ 200
    result.response.reason ≡ "OK"
    shouldSatisfy (result.response.body == "Hello".toUTF8) "body should be Hello"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "301 redirect" := do
  let input := httpMsg [
    "HTTP/1.1 301 Moved Permanently",
    "Location: http://www.example.org/",
    "Content-Length: 0",
    ""
  ]
  match Herald.parseResponse input with
  | .ok result =>
    result.response.status.code ≡ 301
    result.response.reason ≡ "Moved Permanently"
    (result.response.headers.get "Location") ≡ some "http://www.example.org/"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "404 not found" := do
  let input := httpMsg [
    "HTTP/1.1 404 Not Found",
    "Content-Type: text/html",
    "Content-Length: 9",
    "",
    "Not Found"
  ]
  match Herald.parseResponse input with
  | .ok result =>
    result.response.status.code ≡ 404
    result.response.reason ≡ "Not Found"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "500 internal server error" := do
  let input := httpMsg [
    "HTTP/1.1 500 Internal Server Error",
    "Content-Length: 0",
    ""
  ]
  match Herald.parseResponse input with
  | .ok result =>
    result.response.status.code ≡ 500
    shouldSatisfy result.response.status.isServerError "should be server error"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

-- ============================================================================
-- Special Status Codes
-- ============================================================================

test "204 No Content" := do
  let input := httpMsg [
    "HTTP/1.1 204 No Content",
    ""
  ]
  match Herald.parseResponse input with
  | .ok result =>
    result.response.status.code ≡ 204
    result.response.body.size ≡ 0
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "304 Not Modified" := do
  let input := httpMsg [
    "HTTP/1.1 304 Not Modified",
    "ETag: \"abc123\"",
    ""
  ]
  match Herald.parseResponse input with
  | .ok result =>
    result.response.status.code ≡ 304
    result.response.body.size ≡ 0
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "100 Continue" := do
  let input := httpMsg [
    "HTTP/1.1 100 Continue",
    ""
  ]
  match Herald.parseResponse input with
  | .ok result =>
    result.response.status.code ≡ 100
    shouldSatisfy result.response.status.isInformational "should be informational"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "101 Switching Protocols" := do
  let input := httpMsg [
    "HTTP/1.1 101 Switching Protocols",
    "Upgrade: websocket",
    "Connection: Upgrade",
    ""
  ]
  match Herald.parseResponse input with
  | .ok result =>
    result.response.status.code ≡ 101
    result.upgrade ≡ some "websocket"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

-- ============================================================================
-- Chunked Responses
-- ============================================================================

test "chunked response" := do
  let input := httpMsg [
    "HTTP/1.1 200 OK",
    "Transfer-Encoding: chunked",
    "",
    "7",
    "Mozilla",
    "9",
    "Developer",
    "7",
    "Network",
    "0",
    ""
  ]
  match Herald.parseResponse input with
  | .ok result =>
    let bodyStr := String.fromUTF8! result.response.body
    bodyStr ≡ "MozillaDeveloperNetwork"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "chunked with trailers" := do
  let input := httpMsg [
    "HTTP/1.1 200 OK",
    "Transfer-Encoding: chunked",
    "Trailer: Expires",
    "",
    "5",
    "Hello",
    "0",
    "Expires: Wed, 21 Oct 2025 07:28:00 GMT",
    ""
  ]
  match Herald.parseResponse input with
  | .ok result =>
    shouldSatisfy (result.response.body == "Hello".toUTF8) "body should be Hello"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

-- ============================================================================
-- HTTP/1.0 Responses
-- ============================================================================

test "HTTP 1.0 response" := do
  let input := httpMsg [
    "HTTP/1.0 200 OK",
    "Content-Length: 5",
    "",
    "Hello"
  ]
  match Herald.parseResponse input with
  | .ok result =>
    result.response.version.major ≡ 1
    result.response.version.minor ≡ 0
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "HTTP 1.0 with keepalive" := do
  let input := httpMsg [
    "HTTP/1.0 200 OK",
    "Connection: keep-alive",
    "Content-Length: 5",
    "",
    "Hello"
  ]
  match Herald.parseResponse input with
  | .ok result =>
    shouldSatisfy (!result.connectionClose) "should not have connection close"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

-- ============================================================================
-- Edge Cases
-- ============================================================================

test "empty reason phrase" := do
  let input := httpMsg [
    "HTTP/1.1 200 ",
    "Content-Length: 0",
    ""
  ]
  match Herald.parseResponse input with
  | .ok result =>
    result.response.status.code ≡ 200
    result.response.reason ≡ ""
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "no reason phrase space" := do
  let input := httpMsg [
    "HTTP/1.1 200",
    "Content-Length: 0",
    ""
  ]
  match Herald.parseResponse input with
  | .ok result =>
    result.response.status.code ≡ 200
    result.response.reason ≡ ""
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "Connection close" := do
  let input := httpMsg [
    "HTTP/1.1 200 OK",
    "Connection: close",
    "Content-Length: 5",
    "",
    "Hello"
  ]
  match Herald.parseResponse input with
  | .ok result =>
    shouldSatisfy result.connectionClose "should have connection close"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "multiple header values" := do
  let input := httpMsg [
    "HTTP/1.1 200 OK",
    "Set-Cookie: session=abc123",
    "Set-Cookie: user=john",
    "Content-Length: 0",
    ""
  ]
  match Herald.parseResponse input with
  | .ok result =>
    let cookies := result.response.headers.getAll "Set-Cookie"
    cookies.size ≡ 2
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "header with underscore" := do
  let input := httpMsg [
    "HTTP/1.1 200 OK",
    "X_Custom_Header: value",
    "Content-Length: 0",
    ""
  ]
  match Herald.parseResponse input with
  | .ok result =>
    (result.response.headers.get "X_Custom_Header") ≡ some "value"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "various 2xx codes" := do
  for code in [200, 201, 202, 204] do
    let input := httpMsg [
      s!"HTTP/1.1 {code} OK",
      "Content-Length: 0",
      ""
    ]
    match Herald.parseResponse input with
    | .ok result =>
      shouldSatisfy result.response.status.isSuccess s!"{code} should be success"
    | .error e => throw (IO.userError s!"Parse failed for {code}: {e}")

test "various 4xx codes" := do
  for code in [400, 401, 403, 404, 405, 409, 422, 429] do
    let input := httpMsg [
      s!"HTTP/1.1 {code} Error",
      "Content-Length: 0",
      ""
    ]
    match Herald.parseResponse input with
    | .ok result =>
      shouldSatisfy result.response.status.isClientError s!"{code} should be client error"
    | .error e => throw (IO.userError s!"Parse failed for {code}: {e}")

test "various 5xx codes" := do
  for code in [500, 501, 502, 503, 504] do
    let input := httpMsg [
      s!"HTTP/1.1 {code} Error",
      "Content-Length: 0",
      ""
    ]
    match Herald.parseResponse input with
    | .ok result =>
      shouldSatisfy result.response.status.isServerError s!"{code} should be server error"
    | .error e => throw (IO.userError s!"Parse failed for {code}: {e}")

-- ============================================================================
-- Redirection Responses
-- ============================================================================

test "302 Found redirect" := do
  let input := httpMsg [
    "HTTP/1.1 302 Found",
    "Location: /new-location",
    "Content-Length: 0",
    ""
  ]
  match Herald.parseResponse input with
  | .ok result =>
    result.response.status.code ≡ 302
    shouldSatisfy result.response.status.isRedirection "should be redirection"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "307 Temporary Redirect" := do
  let input := httpMsg [
    "HTTP/1.1 307 Temporary Redirect",
    "Location: /temp",
    "Content-Length: 0",
    ""
  ]
  match Herald.parseResponse input with
  | .ok result =>
    result.response.status.code ≡ 307
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "308 Permanent Redirect" := do
  let input := httpMsg [
    "HTTP/1.1 308 Permanent Redirect",
    "Location: /permanent",
    "Content-Length: 0",
    ""
  ]
  match Herald.parseResponse input with
  | .ok result =>
    result.response.status.code ≡ 308
  | .error e => throw (IO.userError s!"Parse failed: {e}")

#generate_tests

end Tests.Parser.Responses

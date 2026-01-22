/-
  Herald Request Parser Tests

  Test cases ported from nodejs/http-parser test suite.
-/
import Herald
import Crucible

namespace Tests.Parser.Requests

open Crucible
open Herald.Core

-- Use Herald.parseRequest to avoid ambiguity

/-- Helper to build HTTP message from lines -/
def httpMsg (lines : List String) : ByteArray :=
  (String.intercalate "\r\n" lines ++ "\r\n").toUTF8

/-- Helper for simpler single-line messages -/
def http (s : String) : ByteArray := s.toUTF8

testSuite "Request Parser"

-- ============================================================================
-- Basic Requests (from http-parser test.c)
-- ============================================================================

test "curl GET" := do
  let input := httpMsg [
    "GET /test HTTP/1.1",
    "User-Agent: curl/7.18.0",
    "Host: 0.0.0.0:5000",
    "Accept: */*",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.method ≡ Method.GET
    result.request.path ≡ "/test"
    result.request.version ≡ Version.http11
    (result.request.headers.get "Host") ≡ some "0.0.0.0:5000"
    (result.request.headers.get "User-Agent") ≡ some "curl/7.18.0"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "firefox GET" := do
  let input := httpMsg [
    "GET /favicon.ico HTTP/1.1",
    "Host: 0.0.0.0:5000",
    "User-Agent: Mozilla/5.0",
    "Accept: text/html,application/xhtml+xml",
    "Accept-Language: en-us,en;q=0.5",
    "Accept-Encoding: gzip,deflate",
    "Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7",
    "Keep-Alive: 300",
    "Connection: keep-alive",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.method ≡ Method.GET
    result.request.path ≡ "/favicon.ico"
    (result.request.headers.get "Keep-Alive") ≡ some "300"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "GET no headers no body" := do
  let input := httpMsg [
    "GET /get_no_headers_no_body/world HTTP/1.1",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.method ≡ Method.GET
    result.request.path ≡ "/get_no_headers_no_body/world"
    result.request.headers.size ≡ 0
    result.request.body.size ≡ 0
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "GET one header no body" := do
  let input := httpMsg [
    "GET /get_one_header_no_body HTTP/1.1",
    "Accept: */*",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.method ≡ Method.GET
    result.request.headers.size ≡ 1
    (result.request.headers.get "Accept") ≡ some "*/*"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "GET with body" := do
  let input := httpMsg [
    "GET /get_funky_content_length_body_hello HTTP/1.0",
    "Content-Length: 5",
    "",
    "HELLO"
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.method ≡ Method.GET
    result.request.version.major ≡ 1
    result.request.version.minor ≡ 0
    shouldSatisfy (result.request.body == "HELLO".toUTF8) "body should be HELLO"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

-- ============================================================================
-- POST Requests
-- ============================================================================

test "POST with body" := do
  let input := httpMsg [
    "POST /post_identity_body_world?q=search#hey HTTP/1.1",
    "Accept: */*",
    "Content-Length: 5",
    "",
    "World"
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.method ≡ Method.POST
    result.request.path ≡ "/post_identity_body_world?q=search#hey"
    shouldSatisfy (result.request.body == "World".toUTF8) "body should be World"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "POST chunked body" := do
  let input := httpMsg [
    "POST /chunked HTTP/1.1",
    "Transfer-Encoding: chunked",
    "",
    "5",
    "Hello",
    "6",
    " World",
    "0",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.method ≡ Method.POST
    let bodyStr := String.fromUTF8! result.request.body
    bodyStr ≡ "Hello World"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "POST chunked with trailing headers" := do
  let input := httpMsg [
    "POST /chunked_w_trailing_headers HTTP/1.1",
    "Transfer-Encoding: chunked",
    "",
    "5",
    "hello",
    "6",
    " world",
    "0",
    "Vary: *",
    "Content-Type: text/plain",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.method ≡ Method.POST
    let bodyStr := String.fromUTF8! result.request.body
    bodyStr ≡ "hello world"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "POST multiple chunks" := do
  let input := httpMsg [
    "POST /two_chunks_mult_zero_end HTTP/1.1",
    "Transfer-Encoding: chunked",
    "",
    "5",
    "hello",
    "6",
    " world",
    "000",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    let bodyStr := String.fromUTF8! result.request.body
    bodyStr ≡ "hello world"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

-- ============================================================================
-- HTTP Methods
-- ============================================================================

test "PUT request" := do
  let input := httpMsg [
    "PUT /resource HTTP/1.1",
    "Content-Length: 4",
    "",
    "data"
  ]
  match Herald.parseRequest input with
  | .ok result => result.request.method ≡ Method.PUT
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "DELETE request" := do
  let input := httpMsg [
    "DELETE /resource HTTP/1.1",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result => result.request.method ≡ Method.DELETE
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "PATCH request" := do
  let input := httpMsg [
    "PATCH /file.txt HTTP/1.1",
    "Content-Type: application/json",
    "Content-Length: 11",
    "",
    "{\"a\": true}"
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.method ≡ Method.PATCH
    result.request.body.size ≡ 11
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "HEAD request" := do
  let input := httpMsg [
    "HEAD /resource HTTP/1.1",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result => result.request.method ≡ Method.HEAD
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "OPTIONS request" := do
  let input := httpMsg [
    "OPTIONS * HTTP/1.1",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.method ≡ Method.OPTIONS
    result.request.path ≡ "*"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "CONNECT request" := do
  let input := httpMsg [
    "CONNECT server.example.com:443 HTTP/1.1",
    "Host: server.example.com:443",
    "Proxy-Authorization: basic aGVsbG86d29ybGQ=",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.method ≡ Method.CONNECT
    result.request.path ≡ "server.example.com:443"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "TRACE request" := do
  let input := httpMsg [
    "TRACE /path HTTP/1.1",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result => result.request.method ≡ Method.TRACE
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "custom REPORT method" := do
  let input := httpMsg [
    "REPORT /report HTTP/1.1",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    match result.request.method with
    | .other name => name ≡ "REPORT"
    | _ => throw (IO.userError "Expected 'other' method")
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "custom PURGE method" := do
  let input := httpMsg [
    "PURGE /cache HTTP/1.1",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    match result.request.method with
    | .other name => name ≡ "PURGE"
    | _ => throw (IO.userError "Expected 'other' method")
  | .error e => throw (IO.userError s!"Parse failed: {e}")

-- ============================================================================
-- Upgrade Requests
-- ============================================================================

test "WebSocket upgrade request" := do
  let input := httpMsg [
    "GET /chat HTTP/1.1",
    "Host: server.example.com",
    "Upgrade: websocket",
    "Connection: Upgrade",
    "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==",
    "Sec-WebSocket-Version: 13",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.method ≡ Method.GET
    shouldSatisfy result.upgrade.isSome "should have upgrade"
    result.upgrade ≡ some "websocket"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "HTTP 2 upgrade request" := do
  let input := httpMsg [
    "GET /resource HTTP/1.1",
    "Host: server.example.com",
    "Connection: Upgrade, HTTP2-Settings",
    "Upgrade: h2c",
    "HTTP2-Settings: AAMAAABkAARAAAAAAAIAAAAA",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.upgrade ≡ some "h2c"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

-- ============================================================================
-- Edge Cases and Special Scenarios
-- ============================================================================

test "request with query string" := do
  let input := httpMsg [
    "GET /test?foo=bar&baz=qux HTTP/1.1",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.path ≡ "/test?foo=bar&baz=qux"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "request with fragment" := do
  let input := httpMsg [
    "GET /test#section HTTP/1.1",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.path ≡ "/test#section"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "request with query and fragment" := do
  let input := httpMsg [
    "GET /test?q=search#posts-17408 HTTP/1.1",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.path ≡ "/test?q=search#posts-17408"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "absolute URI in request" := do
  let input := httpMsg [
    "GET http://example.com/path?query HTTP/1.1",
    "Host: example.com",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.path ≡ "http://example.com/path?query"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "header with empty value" := do
  let input := httpMsg [
    "GET /test HTTP/1.1",
    "X-Empty:",
    "Host: test.com",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    (result.request.headers.get "X-Empty") ≡ some ""
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "header with underscore" := do
  let input := httpMsg [
    "GET /test HTTP/1.1",
    "X_Custom_Header: value",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    (result.request.headers.get "X_Custom_Header") ≡ some "value"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "multiple headers same name" := do
  let input := httpMsg [
    "GET /test HTTP/1.1",
    "Set-Cookie: a=1",
    "Set-Cookie: b=2",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    let cookies := result.request.headers.getAll "Set-Cookie"
    cookies.size ≡ 2
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "leading CRLF keepalive" := do
  let input := http "\r\nGET /test HTTP/1.1\r\n\r\n"
  match Herald.parseRequest input with
  | .ok result =>
    result.request.method ≡ Method.GET
    result.request.path ≡ "/test"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "Connection close" := do
  let input := httpMsg [
    "GET /test HTTP/1.1",
    "Connection: close",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    shouldSatisfy result.connectionClose "should have connection close"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "HTTP 1.0 request" := do
  let input := httpMsg [
    "GET /test HTTP/1.0",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.version.major ≡ 1
    result.request.version.minor ≡ 0
  | .error e => throw (IO.userError s!"Parse failed: {e}")

-- ============================================================================
-- Chunk Encoding Edge Cases
-- ============================================================================

test "chunked with extension" := do
  let input := httpMsg [
    "POST /test HTTP/1.1",
    "Transfer-Encoding: chunked",
    "",
    "5;ext=val",
    "hello",
    "0",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    let bodyStr := String.fromUTF8! result.request.body
    bodyStr ≡ "hello"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "chunked uppercase hex" := do
  let input := httpMsg [
    "POST /test HTTP/1.1",
    "Transfer-Encoding: chunked",
    "",
    "A",
    "0123456789",
    "0",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.body.size ≡ 10
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "chunked lowercase hex" := do
  let input := httpMsg [
    "POST /test HTTP/1.1",
    "Transfer-Encoding: chunked",
    "",
    "a",
    "0123456789",
    "0",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.body.size ≡ 10
  | .error e => throw (IO.userError s!"Parse failed: {e}")

-- ============================================================================
-- Additional Edge Cases (from http-parser test suite)
-- ============================================================================

test "line folding in header value" := do
  let input := http "GET /test HTTP/1.1\r\nX-Folded: first\r\n\tsecond\r\nHost: test.com\r\n\r\n"
  match Herald.parseRequest input with
  | .ok result =>
    (result.request.headers.get "X-Folded") ≡ some "first second"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "line folding with multiple spaces" := do
  let input := http "GET /test HTTP/1.1\r\nX-Folded: line1\r\n   line2\r\n\t line3\r\n\r\n"
  match Herald.parseRequest input with
  | .ok result =>
    (result.request.headers.get "X-Folded") ≡ some "line1 line2 line3"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "hostname with underscore" := do
  let input := httpMsg [
    "GET /test HTTP/1.1",
    "Host: under_score.example.org",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    (result.request.headers.get "Host") ≡ some "under_score.example.org"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "query url with question mark in value" := do
  let input := httpMsg [
    "GET /test?q=what?is+this HTTP/1.1",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.path ≡ "/test?q=what?is+this"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "CONNECT with body" := do
  let input := httpMsg [
    "CONNECT server.example.com:443 HTTP/1.1",
    "Host: server.example.com:443",
    "Content-Length: 10",
    "",
    "0123456789"
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.method ≡ Method.CONNECT
    result.request.body.size ≡ 10
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "LINK request" := do
  let input := httpMsg [
    "LINK /resource HTTP/1.1",
    "Host: example.com",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    match result.request.method with
    | .other name => name ≡ "LINK"
    | _ => throw (IO.userError "Expected 'other' method")
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "UNLINK request" := do
  let input := httpMsg [
    "UNLINK /resource HTTP/1.1",
    "Host: example.com",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    match result.request.method with
    | .other name => name ≡ "UNLINK"
    | _ => throw (IO.userError "Expected 'other' method")
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "M-SEARCH SSDP discovery" := do
  let input := httpMsg [
    "M-SEARCH * HTTP/1.1",
    "HOST: 239.255.255.250:1900",
    "MAN: \"ssdp:discover\"",
    "ST: ssdp:all",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    match result.request.method with
    | .other name => name ≡ "M-SEARCH"
    | _ => throw (IO.userError "Expected 'other' method")
    result.request.path ≡ "*"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "SOURCE request ICE protocol" := do
  let input := httpMsg [
    "SOURCE /stream HTTP/1.0",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    match result.request.method with
    | .other name => name ≡ "SOURCE"
    | _ => throw (IO.userError "Expected 'other' method")
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "apachebench GET" := do
  let input := httpMsg [
    "GET /test HTTP/1.0",
    "Host: test.com",
    "User-Agent: ApacheBench/2.3",
    "Accept: */*",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.method ≡ Method.GET
    result.request.version.major ≡ 1
    result.request.version.minor ≡ 0
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "proxy request absolute URI" := do
  let input := httpMsg [
    "GET http://proxy.example.com:8080/path?query HTTP/1.1",
    "Host: proxy.example.com:8080",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    result.request.path ≡ "http://proxy.example.com:8080/path?query"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "chunked with content length ignored" := do
  -- When both Content-Length and Transfer-Encoding: chunked are present,
  -- chunked takes precedence per HTTP/1.1 spec
  let input := httpMsg [
    "POST /test HTTP/1.1",
    "Content-Length: 100",
    "Transfer-Encoding: chunked",
    "",
    "5",
    "hello",
    "0",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    let bodyStr := String.fromUTF8! result.request.body
    bodyStr ≡ "hello"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "header value with leading whitespace" := do
  let input := httpMsg [
    "GET /test HTTP/1.1",
    "X-Header:   value with leading spaces",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    -- Leading OWS should be trimmed
    (result.request.headers.get "X-Header") ≡ some "value with leading spaces"
  | .error e => throw (IO.userError s!"Parse failed: {e}")

test "header value with trailing whitespace" := do
  let input := httpMsg [
    "GET /test HTTP/1.1",
    "X-Header: value with trailing   ",
    ""
  ]
  match Herald.parseRequest input with
  | .ok result =>
    -- Trailing OWS should be trimmed
    (result.request.headers.get "X-Header") ≡ some "value with trailing"
  | .error e => throw (IO.userError s!"Parse failed: {e}")



end Tests.Parser.Requests

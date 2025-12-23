/-
  Herald Message Parser

  Complete HTTP message parsing: combines request/status line, headers, and body.
-/
import Herald.Parser.RequestLine
import Herald.Parser.StatusLine
import Herald.Parser.Headers
import Herald.Parser.Body
import Herald.Parser.Chunked

namespace Herald.Parser.Message

open Herald.Core
open Decoder

/-- Parsed request with metadata -/
structure ParsedRequest where
  request : Request
  /-- Upgrade protocol if Connection: upgrade and Upgrade header present -/
  upgrade : Option String
  /-- Number of bytes consumed from input -/
  bytesConsumed : Nat
  /-- Whether Connection: close was set -/
  connectionClose : Bool
  deriving Inhabited

/-- Parsed response with metadata -/
structure ParsedResponse where
  response : Response
  /-- Upgrade protocol if this is 101 Switching Protocols -/
  upgrade : Option String
  /-- Number of bytes consumed from input -/
  bytesConsumed : Nat
  /-- Whether Connection: close was set -/
  connectionClose : Bool
  deriving Inhabited

/-- Parse complete HTTP request -/
def parseRequestDecoder : Decoder ParsedRequest := do
  -- Parse request line
  let (method, path, version) ← RequestLine.requestLine

  -- Parse headers
  let hdrs ← Headers.headers

  -- Determine body strategy
  let strategy := Body.requestBodyStrategy method hdrs

  -- Parse body
  let body ← match strategy with
    | .none => pure ByteArray.empty
    | .fixedLength n => Body.fixedLengthBody n
    | .chunked => Chunked.chunkedBodyOnly
    | .untilEof => Body.untilEofBody

  let bytesConsumed ← getPosition

  -- Build request
  let request : Request := {
    method
    path
    version
    headers := hdrs
    body
  }

  -- Check for upgrade
  let upgradeProto := if Headers.isUpgrade hdrs then
    Headers.getUpgrade hdrs
  else
    none

  pure {
    request
    upgrade := upgradeProto
    bytesConsumed
    connectionClose := Headers.isConnectionClose hdrs
  }

/-- Parse complete HTTP response -/
def parseResponseDecoder (requestMethod : Option Method := none) : Decoder ParsedResponse := do
  -- Parse status line
  let (version, status, reason) ← StatusLine.statusLine

  -- Parse headers
  let hdrs ← Headers.headers

  -- Determine body strategy
  let strategy := Body.responseBodyStrategy status hdrs requestMethod

  -- Parse body
  let body ← match strategy with
    | .none => pure ByteArray.empty
    | .fixedLength n => Body.fixedLengthBody n
    | .chunked => Chunked.chunkedBodyOnly
    | .untilEof => Body.untilEofBody

  let bytesConsumed ← getPosition

  -- Build response
  let response : Response := {
    version
    status
    reason
    headers := hdrs
    body
  }

  -- Check for upgrade (101 Switching Protocols)
  let upgradeProto := if status.code == 101 && Headers.hasUpgrade hdrs then
    Headers.getUpgrade hdrs
  else
    none

  pure {
    response
    upgrade := upgradeProto
    bytesConsumed
    connectionClose := Headers.isConnectionClose hdrs
  }

/-- Parse HTTP request from ByteArray -/
def parseRequest (input : ByteArray) : ParseResult ParsedRequest :=
  Decoder.execute parseRequestDecoder input

/-- Parse HTTP request, returning result and bytes consumed -/
def parseRequestWithPos (input : ByteArray) : Except ParseError (ParsedRequest × Nat) :=
  Decoder.executeWithPos parseRequestDecoder input

/-- Parse HTTP response from ByteArray -/
def parseResponse (input : ByteArray) (requestMethod : Option Method := none) : ParseResult ParsedResponse :=
  Decoder.execute (parseResponseDecoder requestMethod) input

/-- Parse HTTP response, returning result and bytes consumed -/
def parseResponseWithPos (input : ByteArray) (requestMethod : Option Method := none) : Except ParseError (ParsedResponse × Nat) :=
  Decoder.executeWithPos (parseResponseDecoder requestMethod) input

end Herald.Parser.Message

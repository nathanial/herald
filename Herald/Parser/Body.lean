/-
  Herald Body Parser

  Parses HTTP message bodies:
  - Fixed-length (Content-Length)
  - Until EOF (Connection: close)
-/
import Herald.Parser.Primitives
import Herald.Parser.Headers

namespace Herald.Parser.Body

open Herald.Core
open Decoder

/-- Body handling strategy -/
inductive BodyStrategy where
  | none           -- No body expected
  | fixedLength (n : Nat)  -- Content-Length specified
  | chunked        -- Transfer-Encoding: chunked
  | untilEof       -- Read until connection closes
  deriving Repr, BEq

/-- Parse fixed-length body -/
def fixedLengthBody (length : Nat) : Decoder ByteArray := do
  readBytes length

/-- Parse body until EOF (read all remaining input) -/
def untilEofBody : Decoder ByteArray := do
  let rem ← remaining
  readBytes rem

/-- Determine body strategy for a request based on method and headers -/
def requestBodyStrategy (method : Method) (hs : Headers) : BodyStrategy :=
  -- Check for Transfer-Encoding: chunked first (takes precedence)
  if Headers.isChunked hs then
    .chunked
  else
    -- Check Content-Length
    match Headers.getContentLength hs with
    | some 0 => .none
    | some n => .fixedLength n
    | none =>
      -- No length info - methods like GET typically have no body
      match method with
      | .GET | .HEAD | .DELETE | .OPTIONS | .TRACE => .none
      | _ => .none  -- Per RFC, request without Content-Length has no body

/-- Determine body strategy for a response -/
def responseBodyStrategy (status : StatusCode) (hs : Headers) (requestMethod : Option Method := none) : BodyStrategy :=
  -- 1xx, 204, 304 responses have no body
  if status.isInformational || status.code == 204 || status.code == 304 then
    .none
  -- Response to HEAD has no body
  else if requestMethod == some .HEAD then
    .none
  -- Check for Transfer-Encoding: chunked
  else if Headers.isChunked hs then
    .chunked
  -- Check Content-Length
  else match Headers.getContentLength hs with
    | some n => .fixedLength n
    | none =>
      -- No length specified - read until EOF for HTTP/1.0 or Connection: close
      if Headers.isConnectionClose hs then
        .untilEof
      else
        -- HTTP/1.1 without length info and not chunked - could be error
        -- but we'll allow reading until EOF
        .untilEof

end Herald.Parser.Body

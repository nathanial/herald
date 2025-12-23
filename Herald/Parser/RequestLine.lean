/-
  Herald Request Line Parser

  Parses HTTP request line: METHOD SP request-target SP HTTP-version CRLF
-/
import Herald.Parser.Primitives

namespace Herald.Parser.RequestLine

open Herald.Core
open Decoder
open Primitives

/-- Parse HTTP method -/
def method : Decoder Method := do
  let tok ← token
  pure (Method.fromString tok)

/-- Parse complete request line -/
def requestLine : Decoder (Method × String × Version) := do
  -- Handle optional leading CRLF (keep-alive connections)
  optLeadingCrlf

  -- METHOD
  let m ← method

  -- SP
  sp

  -- request-target
  let path ← requestTarget

  -- SP (may be optional for HTTP/0.9)
  let hasVersion ← do
    let next ← peekByte
    match next with
    | some b =>
      if b == Ascii.SP then
        let _ ← readByte
        pure true
      else
        pure false
    | none => pure false

  -- HTTP-version (optional for HTTP/0.9)
  let version ← if hasVersion then
    httpVersion
  else
    pure Version.http10  -- Default to 1.0 for version-less requests

  -- CRLF
  crlfOrLf

  pure (m, path, version)

/-- Parse request line, returning detailed error on failure -/
def requestLineWithError : Decoder (Method × String × Version) := do
  try
    requestLine
  catch e =>
    match e with
    | .other msg =>
      if (msg.splitOn "expected token").length > 1 then
        throw (ParseError.invalidMethod "empty or invalid method")
      else if (msg.splitOn "expected CRLF").length > 1 then
        throw (ParseError.other "malformed request line")
      else
        throw e
    | _ => throw e

end Herald.Parser.RequestLine

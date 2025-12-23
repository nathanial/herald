/-
  Herald Status Line Parser

  Parses HTTP status line: HTTP-version SP status-code SP reason-phrase CRLF
-/
import Herald.Parser.Primitives

namespace Herald.Parser.StatusLine

open Herald.Core
open Decoder
open Primitives

/-- Parse reason phrase (may be empty) -/
def reasonPhrase : Decoder String := do
  let next ← peekByte
  match next with
  | some b =>
    if b == Ascii.CR || b == Ascii.LF then
      -- Empty reason phrase is allowed
      pure ""
    else
      restOfLine
  | none => pure ""

/-- Parse complete status line -/
def statusLine : Decoder (Version × StatusCode × String) := do
  -- Handle optional leading CRLF
  optLeadingCrlf

  -- HTTP-version
  let version ← httpVersion

  -- SP
  sp

  -- status-code (3 digits)
  let code ← statusCodeDigits
  let status : StatusCode := { code }

  -- SP (may be missing if reason phrase is empty)
  let hasReason ← do
    let next ← peekByte
    match next with
    | some b =>
      if b == Ascii.SP then
        let _ ← readByte
        pure true
      else
        pure false
    | none => pure false

  -- reason-phrase (may be empty)
  let reason ← if hasReason then reasonPhrase else pure ""

  -- CRLF
  crlfOrLf

  pure (version, status, reason)

/-- Parse status line with detailed errors -/
def statusLineWithError : Decoder (Version × StatusCode × String) := do
  try
    statusLine
  catch e =>
    match e with
    | .invalidVersion msg => throw (.invalidVersion msg)
    | .invalidStatusCode msg => throw (.invalidStatusCode msg)
    | _ => throw e

end Herald.Parser.StatusLine

/-
  Herald Chunked Encoding Parser

  Parses Transfer-Encoding: chunked bodies per RFC 7230:
  - Chunk size (hex)
  - Chunk extensions (optional)
  - Chunk data
  - Trailer headers
-/
import Herald.Parser.Primitives
import Herald.Parser.Headers

namespace Herald.Parser.Chunked

open Herald.Core
open Decoder
open Primitives

/-- Parse chunk extensions (;name=value or ;name) -/
def chunkExtensions : Decoder (Array (String × Option String)) := do
  let mut extensions : Array (String × Option String) := #[]
  let mut done := false

  while !done do
    let next ← peekByte
    match next with
    | some b =>
      if b == Ascii.SEMICOLON then
        let _ ← readByte
        ows
        let name ← token
        ows
        let value ← do
          let eq ← peekByte
          if eq == some Ascii.EQUALS then
            let _ ← readByte
            ows
            -- Value can be token or quoted-string
            let valStart ← peekByte
            if valStart == some Ascii.DQUOTE then
              let v ← quotedString
              pure (some v)
            else
              let v ← token
              pure (some v)
          else
            pure none
        extensions := extensions.push (name, value)
        ows
      else
        done := true
    | none => done := true

  pure extensions

/-- Parse chunk size line: chunk-size [chunk-ext] CRLF -/
def chunkSizeLine : Decoder (Nat × Array (String × Option String)) := do
  -- Read hex digits for size
  let size ← hexNumber

  -- Skip any whitespace after size (some implementations add spaces)
  ows

  -- Parse optional extensions
  let exts ← chunkExtensions

  -- CRLF
  crlfOrLf

  pure (size, exts)

/-- Parse chunk data of given size, followed by CRLF -/
def chunkData (size : Nat) : Decoder ByteArray := do
  let data ← readBytes size
  crlfOrLf
  pure data

/-- Parse single chunk (returns None for final zero-length chunk) -/
def chunk : Decoder (Option ByteArray) := do
  let (size, _) ← chunkSizeLine
  if size == 0 then
    pure none
  else
    let data ← chunkData size
    pure (some data)

/-- Parse trailer headers (after final zero chunk) -/
def trailerHeaders : Decoder Headers := do
  -- Check for blank line (no trailers)
  let blank ← atBlankLine
  if blank then
    crlfOrLf
    pure #[]
  else
    Headers.headers

/-- Parse complete chunked body, returning assembled body and trailers -/
def chunkedBody : Decoder (ByteArray × Headers) := do
  let mut body := ByteArray.empty
  let mut done := false

  -- Read chunks
  while !done do
    let (size, _) ← chunkSizeLine
    if size == 0 then
      done := true
    else
      let data ← chunkData size
      body := body ++ data

  -- Read trailers
  let trailers ← trailerHeaders

  pure (body, trailers)

/-- Parse chunked body, returning only the body (discarding trailers) -/
def chunkedBodyOnly : Decoder ByteArray := do
  let (body, _) ← chunkedBody
  pure body

end Herald.Parser.Chunked

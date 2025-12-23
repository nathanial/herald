/-
  Herald Headers Parser

  Parses HTTP headers with support for:
  - Line folding (obs-fold)
  - Whitespace handling (OWS)
  - Multiple headers with same name
-/
import Herald.Parser.Primitives

namespace Herald.Parser.Headers

open Herald.Core
open Decoder
open Primitives

/-- Parse header field name -/
def headerName : Decoder String := do
  let name ← token
  if name.isEmpty then
    throw (.invalidHeader "empty header name")
  pure name

/-- Parse header field value with line folding support -/
def headerValue : Decoder String := do
  let mut result := ""
  let mut done := false

  while !done do
    -- Read value content until CRLF
    let line ← restOfLine

    -- Check for line folding (obs-fold)
    let fold ← isLineFold

    if fold then
      -- Consume the CRLF and leading whitespace, replace with single space
      crlfOrLf
      ows
      result := result ++ line ++ " "
    else
      result := result ++ line
      done := true

  -- Trim trailing whitespace from value
  pure result.trimRight

/-- Parse single header: field-name ":" OWS field-value OWS -/
def header : Decoder Header := do
  let name ← headerName
  expectByte Ascii.COLON
  ows
  let value ← headerValue
  crlfOrLf
  pure { name, value }

/-- Parse all headers until blank line -/
def headers : Decoder Headers := do
  let mut result : Headers := #[]
  let mut done := false

  while !done do
    let blank ← atBlankLine
    if blank then
      crlfOrLf  -- Consume the blank line
      done := true
    else
      let h ← header
      result := result.push h

  pure result

/-- Extract Content-Length from headers -/
def getContentLength (hs : Headers) : Option Nat :=
  match hs.get "Content-Length" with
  | some s => s.trim.toNat?
  | none => none

/-- Check for Transfer-Encoding: chunked -/
def isChunked (hs : Headers) : Bool :=
  match hs.get "Transfer-Encoding" with
  | some te =>
    -- Check if chunked is the last encoding
    let parts := te.splitOn ","
    match parts.getLast? with
    | some last => last.trim.toLower == "chunked"
    | none => false
  | none => false

/-- Check for Connection: close -/
def isConnectionClose (hs : Headers) : Bool :=
  match hs.get "Connection" with
  | some conn =>
    conn.toLower.splitOn "," |>.any (·.trim == "close")
  | none => false

/-- Check for Connection: keep-alive -/
def isKeepAlive (hs : Headers) : Bool :=
  match hs.get "Connection" with
  | some conn =>
    conn.toLower.splitOn "," |>.any (·.trim == "keep-alive")
  | none => false

/-- Check for Upgrade header -/
def hasUpgrade (hs : Headers) : Bool :=
  hs.get "Upgrade" |>.isSome

/-- Get Upgrade protocol -/
def getUpgrade (hs : Headers) : Option String :=
  hs.get "Upgrade"

/-- Check for Connection: upgrade -/
def isConnectionUpgrade (hs : Headers) : Bool :=
  match hs.get "Connection" with
  | some conn =>
    conn.toLower.splitOn "," |>.any (·.trim == "upgrade")
  | none => false

/-- Check if this is an upgrade request/response -/
def isUpgrade (hs : Headers) : Bool :=
  hasUpgrade hs && isConnectionUpgrade hs

/-- Get Host header (required for HTTP/1.1) -/
def getHost (hs : Headers) : Option String :=
  hs.get "Host"

/-- Get Content-Type header -/
def getContentType (hs : Headers) : Option String :=
  hs.get "Content-Type"

end Herald.Parser.Headers

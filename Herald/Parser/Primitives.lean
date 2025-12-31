/-
  Herald Parser Primitives

  Low-level parsing functions for HTTP message components:
  CRLF, tokens, whitespace, numbers, etc.
-/
import Herald.Parser.Decoder

namespace Herald.Parser

open Herald.Core

-- ASCII character constants
namespace Ascii
  def CR : UInt8 := 13      -- \r
  def LF : UInt8 := 10      -- \n
  def SP : UInt8 := 32      -- space
  def HT : UInt8 := 9       -- horizontal tab
  def COLON : UInt8 := 58   -- :
  def SEMICOLON : UInt8 := 59  -- ;
  def EQUALS : UInt8 := 61  -- =
  def DQUOTE : UInt8 := 34  -- "
  def BACKSLASH : UInt8 := 92  -- \
  def SLASH : UInt8 := 47   -- /
  def QMARK : UInt8 := 63   -- ?
  def HASH : UInt8 := 35    -- #

  def isDigit (b : UInt8) : Bool := b >= 48 && b <= 57  -- 0-9
  def isHexDigit (b : UInt8) : Bool :=
    isDigit b || (b >= 65 && b <= 70) || (b >= 97 && b <= 102)  -- 0-9, A-F, a-f
  def isAlpha (b : UInt8) : Bool :=
    (b >= 65 && b <= 90) || (b >= 97 && b <= 122)  -- A-Z, a-z
  def isAlphaNum (b : UInt8) : Bool := isAlpha b || isDigit b

  /-- HTTP token characters (RFC 7230 section 3.2.6) -/
  def isTokenChar (b : UInt8) : Bool :=
    isAlphaNum b ||
    b == 33  ||  -- !
    b == 35  ||  -- #
    b == 36  ||  -- $
    b == 37  ||  -- %
    b == 38  ||  -- &
    b == 39  ||  -- '
    b == 42  ||  -- *
    b == 43  ||  -- +
    b == 45  ||  -- -
    b == 46  ||  -- .
    b == 94  ||  -- ^
    b == 95  ||  -- _
    b == 96  ||  -- `
    b == 124 ||  -- |
    b == 126     -- ~

  /-- Whitespace: SP or HT -/
  def isWS (b : UInt8) : Bool := b == SP || b == HT

  /-- Visible ASCII characters (0x21-0x7E) -/
  def isVchar (b : UInt8) : Bool := b >= 0x21 && b <= 0x7E

  /-- Printable ASCII (SP through ~) -/
  def isPrint (b : UInt8) : Bool := b >= 0x20 && b <= 0x7E

  -- TODO: Replace with Staple.Hex.hexByteToNat after staple release
  /-- Convert hex digit to value -/
  def hexValue (b : UInt8) : Nat :=
    if b >= 48 && b <= 57 then (b - 48).toNat  -- 0-9
    else if b >= 65 && b <= 70 then (b - 55).toNat  -- A-F
    else if b >= 97 && b <= 102 then (b - 87).toNat  -- a-f
    else 0
end Ascii

namespace Primitives

open Decoder

/-- Parse CRLF (strict) -/
def crlf : Decoder Unit := do
  expectByte Ascii.CR
  expectByte Ascii.LF

/-- Parse CRLF or just LF (lenient - common in practice) -/
def crlfOrLf : Decoder Unit := do
  let b ← readByte
  if b == Ascii.CR then
    expectByte Ascii.LF
  else if b != Ascii.LF then
    throw (.other s!"expected CRLF or LF, got {b}")

/-- Parse optional leading CRLF (for keep-alive connections) -/
def optLeadingCrlf : Decoder Unit := do
  let next ← peekByte
  match next with
  | some b =>
    if b == Ascii.CR then
      let _ ← readByte
      let next2 ← peekByte
      if next2 == some Ascii.LF then
        let _ ← readByte
      else
        -- Put back by adjusting position
        let s ← get
        set { s with position := s.position - 1 }
    else if b == Ascii.LF then
      let _ ← readByte
  | none => pure ()

/-- Parse a single space -/
def sp : Decoder Unit := expectByte Ascii.SP

/-- Parse optional whitespace (OWS = *(SP / HTAB)) -/
def ows : Decoder Unit := do
  let _ ← readWhile Ascii.isWS
  pure ()

/-- Parse required whitespace (RWS = 1*(SP / HTAB)) -/
def rws : Decoder Unit := do
  let ws ← readWhile Ascii.isWS
  if ws.isEmpty then
    throw (.other "expected whitespace")

/-- Parse HTTP token (method name, header field name, etc.) -/
def token : Decoder String := do
  let bytes ← readWhile Ascii.isTokenChar
  if bytes.isEmpty then
    throw (.other "expected token")
  bytesToString bytes

/-- Parse decimal number -/
def decimal : Decoder Nat := do
  let bytes ← readWhile Ascii.isDigit
  if bytes.isEmpty then
    throw (.other "expected decimal number")
  let s ← bytesToString bytes
  match s.toNat? with
  | some n => pure n
  | none => throw (.other s!"invalid decimal: {s}")

/-- Parse 3-digit status code -/
def statusCodeDigits : Decoder UInt16 := do
  let bytes ← readBytes 3
  for b in bytes.toList do
    if !Ascii.isDigit b then
      throw (.invalidStatusCode s!"non-digit in status code")
  let s ← bytesToString bytes
  match s.toNat? with
  | some n => pure n.toUInt16
  | none => throw (.invalidStatusCode s)

/-- Parse hexadecimal number (for chunk sizes) -/
def hexNumber : Decoder Nat := do
  let bytes ← readWhile Ascii.isHexDigit
  if bytes.isEmpty then
    throw (.invalidChunkSize)
  let mut result : Nat := 0
  for b in bytes.toList do
    result := result * 16 + Ascii.hexValue b
  pure result

/-- Parse HTTP version: "HTTP/" DIGIT "." DIGIT -/
def httpVersion : Decoder Version := do
  expectBytes "HTTP/".toUTF8
  let major ← readByte
  if !Ascii.isDigit major then
    throw (.invalidVersion "expected major version digit")
  expectByte 46  -- '.'
  let minor ← readByte
  if !Ascii.isDigit minor then
    throw (.invalidVersion "expected minor version digit")
  pure { major := major - 48, minor := minor - 48 }

/-- Parse optional HTTP version (for HTTP/0.9 requests) -/
def optHttpVersion : Decoder (Option Version) := do
  let next ← peekBytes 5
  match next with
  | some bytes =>
    if bytes == "HTTP/".toUTF8 then
      let v ← httpVersion
      pure (some v)
    else
      pure none
  | none => pure none

/-- Read rest of line until CRLF (not consuming CRLF) -/
def restOfLine : Decoder String := do
  let bytes ← readUntil (fun b => b == Ascii.CR || b == Ascii.LF)
  bytesToString bytes

/-- Read rest of line until CRLF, then consume CRLF -/
def restOfLineWithCrlf : Decoder String := do
  let s ← restOfLine
  crlfOrLf
  pure s

/-- Check if at a blank line (CRLF immediately follows) -/
def atBlankLine : Decoder Bool := do
  let next ← peekBytes 2
  match next with
  | some bytes =>
    pure (bytes.get! 0 == Ascii.CR && bytes.get! 1 == Ascii.LF)
  | none =>
    let next1 ← peekByte
    pure (next1 == some Ascii.LF)

/-- Check for line folding (obs-fold): CRLF followed by SP or HT -/
def isLineFold : Decoder Bool := do
  let next ← peekBytes 3
  match next with
  | some bytes =>
    if bytes.get! 0 == Ascii.CR && bytes.get! 1 == Ascii.LF then
      pure (Ascii.isWS (bytes.get! 2))
    else if bytes.get! 0 == Ascii.LF then
      pure (Ascii.isWS (bytes.get! 1))
    else
      pure false
  | none => pure false

/-- Parse quoted string (RFC 7230 quoted-string) -/
def quotedString : Decoder String := do
  expectByte Ascii.DQUOTE
  let mut result := ByteArray.empty
  let mut done := false
  while !done do
    let b ← readByte
    if b == Ascii.DQUOTE then
      done := true
    else if b == Ascii.BACKSLASH then
      -- Quoted-pair: backslash followed by any VCHAR or SP/HT
      let escaped ← readByte
      result := result.push escaped
    else
      result := result.push b
  bytesToString result

/-- Parse request target (path with query string) -/
def requestTarget : Decoder String := do
  -- Request-target is everything until SP or CRLF
  let bytes ← readUntil (fun b => b == Ascii.SP || b == Ascii.CR || b == Ascii.LF)
  if bytes.isEmpty then
    throw .invalidPath
  bytesToString bytes

end Primitives

end Herald.Parser

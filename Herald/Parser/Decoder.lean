/-
  Herald Parser Decoder Monad

  The decoder combines ExceptT for error handling with StateM for position
  tracking. Based on the proven Protolean.Decoder pattern.
-/
import Herald.Core

namespace Herald.Parser

open Herald.Core

/-- Decoder state tracking position in input -/
structure DecoderState where
  input : ByteArray
  position : Nat
  limit : Option Nat  -- For bounded parsing
  deriving Inhabited

/-- Decoder monad combining state and error handling -/
abbrev Decoder := ExceptT ParseError (StateM DecoderState)

namespace Decoder

/-- Run decoder on input bytes -/
def execute (dec : Decoder α) (input : ByteArray) : ParseResult α :=
  let (result, _) := ExceptT.run dec { input, position := 0, limit := none }
  result

/-- Run decoder and also return final position -/
def executeWithPos (dec : Decoder α) (input : ByteArray) : Except ParseError (α × Nat) :=
  let (result, state) := ExceptT.run dec { input, position := 0, limit := none }
  match result with
  | .ok a => .ok (a, state.position)
  | .error e => .error e

/-- Run decoder starting at a specific position -/
def executeFrom (dec : Decoder α) (input : ByteArray) (startPos : Nat) : ParseResult α :=
  let (result, _) := ExceptT.run dec { input, position := startPos, limit := none }
  result

/-- Get the effective end position (limit or input size) -/
def getEndPos : Decoder Nat := do
  let s ← get
  return s.limit.getD s.input.size

/-- Check if we've reached end of input (or limit) -/
def atEnd : Decoder Bool := do
  let s ← get
  let endPos ← getEndPos
  return s.position >= endPos

/-- Get remaining bytes count -/
def remaining : Decoder Nat := do
  let s ← get
  let endPos ← getEndPos
  return endPos - s.position

/-- Get current position -/
def getPosition : Decoder Nat := do
  let s ← get
  return s.position

/-- Get the full input ByteArray -/
def getInput : Decoder ByteArray := do
  let s ← get
  return s.input

/-- Read a single byte -/
def readByte : Decoder UInt8 := do
  let s ← get
  let endPos ← getEndPos
  if s.position >= endPos then
    throw .incomplete
  let byte := s.input.get! s.position
  set { s with position := s.position + 1 }
  return byte

/-- Peek at the next byte without consuming it -/
def peekByte : Decoder (Option UInt8) := do
  let s ← get
  let endPos ← getEndPos
  if s.position >= endPos then
    return none
  return some (s.input.get! s.position)

/-- Peek at the next n bytes without consuming them -/
def peekBytes (n : Nat) : Decoder (Option ByteArray) := do
  let s ← get
  let endPos ← getEndPos
  if s.position + n > endPos then
    return none
  return some (s.input.extract s.position (s.position + n))

/-- Read exactly n bytes -/
def readBytes (n : Nat) : Decoder ByteArray := do
  let s ← get
  let endPos ← getEndPos
  if s.position + n > endPos then
    throw .incomplete
  let bytes := s.input.extract s.position (s.position + n)
  set { s with position := s.position + n }
  return bytes

/-- Skip n bytes -/
def skip (n : Nat) : Decoder Unit := do
  let s ← get
  let endPos ← getEndPos
  if s.position + n > endPos then
    throw .incomplete
  set { s with position := s.position + n }

/-- Read bytes until predicate returns true on a byte (exclusive - stops before matching byte) -/
def readUntil (pred : UInt8 → Bool) : Decoder ByteArray := do
  let s ← get
  let endPos ← getEndPos
  let mut i := s.position
  while i < endPos do
    if pred (s.input.get! i) then
      let bytes := s.input.extract s.position i
      set { s with position := i }
      return bytes
    i := i + 1
  -- Reached end without finding delimiter
  throw .incomplete

/-- Read bytes until a specific byte is found (exclusive) -/
def readUntilByte (b : UInt8) : Decoder ByteArray :=
  readUntil (· == b)

/-- Read bytes while predicate returns true -/
def readWhile (pred : UInt8 → Bool) : Decoder ByteArray := do
  let s ← get
  let endPos ← getEndPos
  let mut i := s.position
  while i < endPos && pred (s.input.get! i) do
    i := i + 1
  let bytes := s.input.extract s.position i
  set { s with position := i }
  return bytes

/-- Expect and consume a specific byte -/
def expectByte (expected : UInt8) : Decoder Unit := do
  let b ← readByte
  if b != expected then
    throw (.other s!"expected byte {expected}, got {b}")

/-- Expect and consume a byte sequence -/
def expectBytes (expected : ByteArray) : Decoder Unit := do
  let actual ← readBytes expected.size
  if actual != expected then
    throw (.other s!"expected bytes {expected.toList}, got {actual.toList}")

/-- Try a decoder, returning none if it fails without consuming input -/
def tryPeek (dec : Decoder α) : Decoder (Option α) := do
  let startPos ← getPosition
  try
    let result ← dec
    return some result
  catch _ =>
    let s ← get
    set { s with position := startPos }
    return none

/-- Run decoder within a length limit -/
def withLimit (length : Nat) (dec : Decoder α) : Decoder α := do
  let s ← get
  let oldLimit := s.limit
  let newLimit := s.position + length
  let effectiveLimit := min newLimit (oldLimit.getD s.input.size)
  set { s with limit := some effectiveLimit }
  let result ← dec
  let s' ← get
  set { s' with limit := oldLimit }
  return result

/-- Convert ByteArray to String (UTF-8) -/
def bytesToString (bytes : ByteArray) : Decoder String :=
  match String.fromUTF8? bytes with
  | some s => pure s
  | none => throw (.other "invalid UTF-8")

/-- Read bytes until delimiter, convert to string -/
def readStringUntil (pred : UInt8 → Bool) : Decoder String := do
  let bytes ← readUntil pred
  bytesToString bytes

/-- Read bytes while predicate, convert to string -/
def readStringWhile (pred : UInt8 → Bool) : Decoder String := do
  let bytes ← readWhile pred
  bytesToString bytes

end Decoder

end Herald.Parser

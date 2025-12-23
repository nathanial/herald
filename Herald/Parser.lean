/-
  Herald HTTP Parser

  Main module that re-exports the public parsing API.
-/
import Herald.Parser.Decoder
import Herald.Parser.Primitives
import Herald.Parser.RequestLine
import Herald.Parser.StatusLine
import Herald.Parser.Headers
import Herald.Parser.Body
import Herald.Parser.Chunked
import Herald.Parser.Message

namespace Herald

-- Re-export main parsing functions
open Parser.Message

/-- Parse an HTTP request from bytes -/
def parseRequest (input : ByteArray) : Core.ParseResult Parser.Message.ParsedRequest :=
  Parser.Message.parseRequest input

/-- Parse an HTTP response from bytes -/
def parseResponse (input : ByteArray) (requestMethod : Option Core.Method := none) : Core.ParseResult Parser.Message.ParsedResponse :=
  Parser.Message.parseResponse input requestMethod

/-- Helper to create ByteArray from String -/
def httpBytes (s : String) : ByteArray := s.toUTF8

end Herald

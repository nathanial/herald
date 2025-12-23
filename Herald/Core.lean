/-
  Herald Core Types
  Basic HTTP message types and parsing primitives.
-/

namespace Herald.Core

/-- HTTP request methods -/
inductive Method where
  | GET
  | POST
  | PUT
  | DELETE
  | PATCH
  | HEAD
  | OPTIONS
  | TRACE
  | CONNECT
  | other (name : String)
  deriving Repr, Inhabited, BEq

namespace Method

def toString : Method → String
  | GET => "GET"
  | POST => "POST"
  | PUT => "PUT"
  | DELETE => "DELETE"
  | PATCH => "PATCH"
  | HEAD => "HEAD"
  | OPTIONS => "OPTIONS"
  | TRACE => "TRACE"
  | CONNECT => "CONNECT"
  | other name => name

instance : ToString Method := ⟨toString⟩

def fromString (s : String) : Method :=
  match s.toUpper with
  | "GET" => GET
  | "POST" => POST
  | "PUT" => PUT
  | "DELETE" => DELETE
  | "PATCH" => PATCH
  | "HEAD" => HEAD
  | "OPTIONS" => OPTIONS
  | "TRACE" => TRACE
  | "CONNECT" => CONNECT
  | _ => other s

end Method

/-- HTTP status codes -/
structure StatusCode where
  code : UInt16
  deriving Repr, Inhabited, BEq

namespace StatusCode

-- Informational 1xx
def continue_ : StatusCode := ⟨100⟩
def switchingProtocols : StatusCode := ⟨101⟩

-- Success 2xx
def ok : StatusCode := ⟨200⟩
def created : StatusCode := ⟨201⟩
def accepted : StatusCode := ⟨202⟩
def noContent : StatusCode := ⟨204⟩

-- Redirection 3xx
def movedPermanently : StatusCode := ⟨301⟩
def found : StatusCode := ⟨302⟩
def seeOther : StatusCode := ⟨303⟩
def notModified : StatusCode := ⟨304⟩
def temporaryRedirect : StatusCode := ⟨307⟩
def permanentRedirect : StatusCode := ⟨308⟩

-- Client Error 4xx
def badRequest : StatusCode := ⟨400⟩
def unauthorized : StatusCode := ⟨401⟩
def forbidden : StatusCode := ⟨403⟩
def notFound : StatusCode := ⟨404⟩
def methodNotAllowed : StatusCode := ⟨405⟩
def conflict : StatusCode := ⟨409⟩
def gone : StatusCode := ⟨410⟩
def unprocessableEntity : StatusCode := ⟨422⟩
def tooManyRequests : StatusCode := ⟨429⟩

-- Server Error 5xx
def internalServerError : StatusCode := ⟨500⟩
def notImplemented : StatusCode := ⟨501⟩
def badGateway : StatusCode := ⟨502⟩
def serviceUnavailable : StatusCode := ⟨503⟩
def gatewayTimeout : StatusCode := ⟨504⟩

def isInformational (s : StatusCode) : Bool := s.code >= 100 && s.code < 200
def isSuccess (s : StatusCode) : Bool := s.code >= 200 && s.code < 300
def isRedirection (s : StatusCode) : Bool := s.code >= 300 && s.code < 400
def isClientError (s : StatusCode) : Bool := s.code >= 400 && s.code < 500
def isServerError (s : StatusCode) : Bool := s.code >= 500 && s.code < 600
def isError (s : StatusCode) : Bool := s.code >= 400

/-- Get the default reason phrase for a status code -/
def defaultReason (s : StatusCode) : String :=
  match s.code with
  | 100 => "Continue"
  | 101 => "Switching Protocols"
  | 200 => "OK"
  | 201 => "Created"
  | 202 => "Accepted"
  | 204 => "No Content"
  | 301 => "Moved Permanently"
  | 302 => "Found"
  | 303 => "See Other"
  | 304 => "Not Modified"
  | 307 => "Temporary Redirect"
  | 308 => "Permanent Redirect"
  | 400 => "Bad Request"
  | 401 => "Unauthorized"
  | 403 => "Forbidden"
  | 404 => "Not Found"
  | 405 => "Method Not Allowed"
  | 409 => "Conflict"
  | 410 => "Gone"
  | 422 => "Unprocessable Entity"
  | 429 => "Too Many Requests"
  | 500 => "Internal Server Error"
  | 501 => "Not Implemented"
  | 502 => "Bad Gateway"
  | 503 => "Service Unavailable"
  | 504 => "Gateway Timeout"
  | _ => ""

instance : ToString StatusCode := ⟨fun s => toString s.code⟩

end StatusCode

/-- HTTP version -/
structure Version where
  major : UInt8
  minor : UInt8
  deriving Repr, Inhabited, BEq

namespace Version

def http10 : Version := ⟨1, 0⟩
def http11 : Version := ⟨1, 1⟩

def toString (v : Version) : String :=
  s!"HTTP/{v.major}.{v.minor}"

instance : ToString Version := ⟨toString⟩

end Version

/-- A single HTTP header -/
structure Header where
  name : String
  value : String
  deriving Repr, Inhabited, BEq

/-- Collection of HTTP headers -/
abbrev Headers := Array Header

namespace Headers

def empty : Headers := #[]

def add (headers : Headers) (name value : String) : Headers :=
  headers.push { name, value }

def get (headers : Headers) (name : String) : Option String :=
  let nameLower := name.toLower
  headers.find? (fun h => h.name.toLower == nameLower) |>.map (·.value)

def getAll (headers : Headers) (name : String) : Array String :=
  let nameLower := name.toLower
  headers.filterMap fun h =>
    if h.name.toLower == nameLower then some h.value else none

end Headers

/-- Parse errors -/
inductive ParseError where
  | incomplete
  | invalidMethod (s : String)
  | invalidPath
  | invalidVersion (s : String)
  | invalidStatusCode (s : String)
  | invalidHeader (line : String)
  | invalidChunkSize
  | messageTooLarge
  | other (msg : String)
  deriving Repr, Inhabited, BEq

namespace ParseError

def toString : ParseError → String
  | incomplete => "Incomplete message"
  | invalidMethod s => s!"Invalid method: {s}"
  | invalidPath => "Invalid request path"
  | invalidVersion s => s!"Invalid HTTP version: {s}"
  | invalidStatusCode s => s!"Invalid status code: {s}"
  | invalidHeader line => s!"Invalid header: {line}"
  | invalidChunkSize => "Invalid chunk size"
  | messageTooLarge => "Message too large"
  | other msg => msg

instance : ToString ParseError := ⟨toString⟩

end ParseError

/-- Result type for parsing operations -/
abbrev ParseResult (α : Type) := Except ParseError α

/-- HTTP request -/
structure Request where
  method : Method
  path : String
  version : Version
  headers : Headers
  body : ByteArray
  deriving Inhabited

/-- HTTP response -/
structure Response where
  version : Version
  status : StatusCode
  reason : String
  headers : Headers
  body : ByteArray
  deriving Inhabited

end Herald.Core

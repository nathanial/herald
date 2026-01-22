/-
  Herald Test Suite
  Main entry point for running all tests.
-/

import Herald
import Crucible
import Tests.Parser.Requests
import Tests.Parser.Responses

-- Core type tests are defined in their own namespace
namespace Tests.Core

open Crucible
open Herald.Core

testSuite "Method"

test "Method fromString parses GET" :=
  Method.fromString "GET" ≡ Method.GET

test "Method fromString parses POST" :=
  Method.fromString "POST" ≡ Method.POST

test "Method fromString is case insensitive" :=
  Method.fromString "get" ≡ Method.GET

test "Method fromString handles unknown methods" := do
  let m := Method.fromString "CUSTOM"
  match m with
  | .other name => name ≡ "CUSTOM"
  | _ => throw (IO.userError "Expected other")

test "Method toString roundtrips" := do
  let methods := [Method.GET, Method.POST, Method.PUT, Method.DELETE]
  for m in methods do
    Method.fromString m.toString ≡ m

testSuite "StatusCode"

test "StatusCode ok is 200" :=
  StatusCode.ok.code ≡ 200

test "StatusCode notFound is 404" :=
  StatusCode.notFound.code ≡ 404

test "StatusCode isSuccess for 200" :=
  shouldSatisfy StatusCode.ok.isSuccess "200 should be success"

test "StatusCode isClientError for 404" :=
  shouldSatisfy StatusCode.notFound.isClientError "404 should be client error"

test "StatusCode isServerError for 500" :=
  shouldSatisfy StatusCode.internalServerError.isServerError "500 should be server error"

test "StatusCode isError for 4xx and 5xx" := do
  shouldSatisfy StatusCode.badRequest.isError "400 should be error"
  shouldSatisfy StatusCode.internalServerError.isError "500 should be error"
  shouldSatisfy (!StatusCode.ok.isError) "200 should not be error"

testSuite "Version"

test "Version http11 is 1.1" := do
  Version.http11.major ≡ 1
  Version.http11.minor ≡ 1

test "Version toString formats correctly" :=
  Version.http11.toString ≡ "HTTP/1.1"

testSuite "Headers"

test "Headers empty is empty" :=
  Headers.empty.size ≡ 0

test "Headers add adds header" := do
  let headers := Headers.empty.add "Content-Type" "application/json"
  headers.size ≡ 1

test "Headers get retrieves header" := do
  let headers := Headers.empty.add "Content-Type" "application/json"
  headers.get "Content-Type" ≡ some "application/json"

test "Headers get is case insensitive" := do
  let headers := Headers.empty.add "Content-Type" "application/json"
  headers.get "content-type" ≡ some "application/json"

test "Headers get returns none for missing" := do
  let headers := Headers.empty.add "Content-Type" "application/json"
  headers.get "Accept" ≡ none

test "Headers getAll retrieves all values" := do
  let headers := Headers.empty
    |>.add "Set-Cookie" "a=1"
    |>.add "Set-Cookie" "b=2"
  let cookies := headers.getAll "Set-Cookie"
  cookies.size ≡ 2



end Tests.Core

-- Main entry point
open Crucible

def main : IO UInt32 := do
  IO.println "Herald HTTP Parser Tests"
  IO.println "========================"
  IO.println ""

  let result ← runAllSuites

  IO.println ""
  if result != 0 then
    IO.println "Some tests failed!"
    return 1
  else
    IO.println "All tests passed!"
    return 0

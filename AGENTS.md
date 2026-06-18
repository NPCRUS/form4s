# AGENTS.md

## Project

form4s — a Scala 3 library for type-safe web form rendering, decoding, and validation. Pure library, no main entry point.

## Build

- **Build tool:** Mill 1.1.6 (use `./mill` launcher)
- **Scala:** 3.8.4
- **Module:** `form4s` (single module)

### Commands

| Task | Command |
|---|---|
| Compile | `./mill form4s.compile` |
| Test | `./mill test` |
| Format | `./mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources` |
| Format check | `./mill mill.scalalib.scalafmt.ScalafmtModule/checkFormatAll __.sources` |

## Dependencies

- `magnolia` 1.3.21 — typeclass auto-derivation for case classes
- `zio` 2.1.26 — effect system
- `zio-http` 3.11.2 — HTTP form parsing

## Source Layout

```
form4s/src/
  Form.scala        # package form — core Form[Out] trait, FieldSchema, Renderable, draw/validate
  FormDecoder.scala  # package util — FormDecoder typeclass with Magnolia auto-derivation, decodes zio-http Form
  Validator.scala    # package form — composable validators (error messages in Russian)
```

## Architecture

- **`Form[Out]`** — tagless-final algebra parameterized by output type. Users implement for their rendering target (HTML, etc.). Includes `decodeAndValidate` combining `FormDecoder` + `Validator` into `Either[Map[String, Seq[String]], T]`.
- **`FormDecoder[T]`** — typeclass decoded from `zio.http.Form`. Uses Magnolia `AutoDerivation` for case classes. Returns `Either[Seq[DecodingError], T]`. Sealed traits unsupported.
- **`DecodingError(field, message)`** — structured decode error; `field` is populated inside `join` (Magnolia case class derivation).
- **`Validator[T]`** — simple `validate(in: T): Seq[String]` with combinators (`compose`, `empty`).

## Conventions

- Scala 3 features: given instances, type lambdas, higher-kinded types, match expressions
- scalafmt 3.10.7 with `runner.dialect = scala3`
- No comments in code — keep it minimal
- Validator error messages are in Russian
- Test framework: utest 0.8.9 (`test` module, sources in `test/src/`)
- E2E tests use Playwright 1.60.0 (headless Chromium) + zio-http server
- Playwright browser install: `./mill test.runMain com.microsoft.playwright.CLI install chromium`

## MCP Servers

- **Metals** (Scala LSP) — remote at `http://127.0.0.1:33215/mcp`
- **BrowserMCP** — local via `npx @browsermcp/mcp@latest`

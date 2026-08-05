# AGENTS.md

## Project

form4s — a Scala 3 library for type-safe web form rendering, decoding, and validation. Library has no entry point; `demo` module contains a runnable zio-http demo app.

## Build

- **Build tool:** Mill 1.1.7 (use `./mill` launcher; `//| mill-version: 1.1.7` in `build.mill`)
- **Scala:** 3.8.4
- **Modules:** `form4s` (library, `PublishModule`, org `io.github.npcrus`), `demo` (depends on `form4s`), `test` (depends on `form4s`)

### Commands

| Task | Command |
|---|---|
| Compile | `./mill form4s.compile` |
| Test | `./mill test` |
| Format | `./mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources` |
| Format check | `./mill mill.scalalib.scalafmt.ScalafmtModule/checkFormatAll __.sources` |
| Run demo | `./mill demo.run` |

## Dependencies

- `magnolia` 1.3.21 — typeclass auto-derivation for case classes
- `zio` 2.1.26 — effect system
- `zio-http` 3.11.2 — HTTP form parsing

## Source Layout

```
form4s/src/          # package form4s
  Form.scala         # Form[Elem] trait, FormSchema enum, FieldSchema, Renderable, draw/validate/decodeAndValidate, IncompleteForm
  FormDecoder.scala  # FormDecoder typeclass with Magnolia auto-derivation, decodes zio-http Form
  Validator.scala    # pure composable validators (error messages in Russian)
  ValidatorZIO.scala # effectful validators used by FieldSchema
  Cursor.scala       # dot-notation path builder for field names and error keys
demo/src/            # package demo — DemoApp (zio-http server, port 8080), DemoHtmlForm
test/src/            # package form4s — utest suites; HtmlForm implements Form[Dom] (zio-http template2)
```

## Architecture

- **`Form[Elem]`** — rendering algebra parameterized by element type (`base`, `amend`, `render`, subform containers `subFormContainer`/`listOfSubformsContainer`, `addBtn`/`deleteBtn`). Hosts path-dependent types: `Renderable[T]`, `FieldSchema[T]`, and `FormSchema[T]` enum with cases `Field`, `SubForm`, `RepeatedSubForm`. Provides `draw` (re-render with old values + errors), `validate`, and `decodeAndValidate`.
- **HKT pattern** — user forms are `case class X[F[_]](...)`; raw data is `X[[T] =>> T]`, schema is `X[FormSchema]` (see `test/src/DecodeAndValidateTests.scala`).
- **`decodeAndValidate`** — runs `FormDecoder.decode`, then `validate`; returns `ZIO[Any, IncompleteForm[T], T]`. `IncompleteForm(errors, oldForm)`: error keys are dot-notation paths; `oldForm` is `Some(decoded)` on validation failure, `None` on decode failure.
- **`FormDecoder[T]`** — typeclass decoding `zio.http.Form` into `Either[Seq[DecodingError], T]`. Magnolia `AutoDerivation` for case classes; `split` supports parameterless enums by variant name (non-enum sealed traits error). Primitive decoders: String, Int, Long, Double, Float, BigDecimal, UUID, Boolean, LocalDateTime; containers: Option, Seq (comma-separated values, repeated fields with same key, indexed dot-notation incl. nested subforms), Either. `decodeFormData` decodes multipart bodies (with `octet-stream`-as-text workaround). Missing non-optional field → "Обязательное поле".
- **`DecodingError(field, message)`** — structured decode error. Field paths propagate through nested subforms via `prependField`: dot-notation (`address.city`) and indexed (`docs.0.typ`); errors on missing entire subform point at the subform field itself (`address`).
- **`Validator[T]`** — pure `validate(in: T): Seq[String]`; combinators `compose`, `empty`, `contramap`, `map`, `option`, `toZIO`; presets `nonEmpty`, `required`, `minLength`, `maxLength`, `isEmail`, `isPhone`, `matches`, `min`, `max`, `requiredTrue`, `positive`, `isUrl`, `custom`.
- **`ValidatorZIO[T]`** — effectful `validate(in: T): ZIO[Any, Nothing, Seq[String]]`; `FieldSchema.validator` holds this type; pure validators lift via `toZIO`.
- **`Cursor`** — accumulates path segments during `draw`/`validate`; `build` renders dot-notation strings matching form field names and error keys.

## Conventions

- Scala 3 features: given instances, type lambdas, higher-kinded types, match expressions
- scalafmt 3.10.7 with `runner.dialect = scala3`
- ScalaDoc comments on all public API entities (types, methods, given instances)
- Validator error messages are in Russian; FormDecoder messages are too ("Обязательное поле", "Невозможно преобразовать в число", etc.)
- Test framework: utest 0.8.9 (`test` module, sources in `test/src/`)
- E2E tests use Playwright 1.60.0 (headless Chromium) + zio-http server
- Playwright browser install: `./mill test.runMain com.microsoft.playwright.CLI install chromium`

## MCP Servers

- **Metals** (Scala LSP) — remote at `http://127.0.0.1:33215/mcp` (config in `opencode.jsonc`). Useful tools: `compile-full`/`compile-module`, `test`, `format-file`, `get-source`/`get-docs`/`inspect`, `get-usages`, `glob-search`
- **BrowserMCP** — local via `npx @browsermcp/mcp@latest`

<!-- keep-the-why:config -->
- context: `context/`
- init: complete
- context-schema: 0.6.4
- capture-confirmation: confirm-always
- source-reference: never
<!-- /keep-the-why:config -->

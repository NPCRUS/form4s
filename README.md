# form4s

Type-safe web form **rendering**, **decoding**, and **validation** for Scala 3,
built on optics. Field names are derived from the compiler — never hardcoded —
and stay in sync across all three concerns automatically.

```scala
//> using dep io.github.npcrus::form4s:0.0.6
```

## The idea

A form has three orthogonal jobs, and they all need to agree on *which field is
which*. form4s makes a single optic the source of field identity:

- **Optics** — `LeafField[S, A, N]` (name + get/replace), derived via
  `field(_.name)` or `AutoFields`.
- **Decoding** — `FieldDecoder` (string → value) and `FormDecoder` (a
  `zio.http.Form` → typed value), auto-derived for case classes.
- **Validation** — `Validator` (pure) / `ValidatorZIO` (effectful) lifted into
  rules (`rule`/`whole`/`ruleEach`) assembled into a `Validation`.
- **Rendering** — `draw`/`drawField` produce a `FormRenderer`; `draw` also
  enforces *at compile time* that every field is drawn exactly once. The
  library is element-agnostic — you pick the output type (e.g. `List[Dom]`).
- **Pipeline** — `decodeAndValidate` ties decode + validate together, failing
  with `Incomplete` so you can re-render.

## Quick start

```scala
import zio.blocks.schema.Schema
import zio.http.template2.*

case class Registration(username: String, age: Option[Int])
object Registration extends AutoFields[Registration]:
  given Schema[Registration] = Schema.derived

given FormDecoder[Registration] = FormDecoder.derived

given Validation[Registration] = Validation(
  rule(Registration.username, Validator.nonEmpty),
  rule(Registration.age, (Validator.min(1) ++ Validator.max(150)).option)
)

val form: FormRenderer[Registration, List[Dom]] =
  draw(
    drawField(Registration.username)(ctx => List[Dom](input(name := ctx.path))),
    drawField(Registration.age)(ctx => List[Dom](input(name := ctx.path)))
  )(_ ++ _)

// Initial render:    renderForm(form, None, Map.empty)
// After a bad submit: renderForm(form, incomplete.oldForm, incomplete.errors)
// On submit:         decodeAndValidate[Registration](formData)
```

## Public API map

### Optics

| Symbol | Purpose |
|---|---|
| `Field[S, A]` | typed access path: `name` + `get` |
| `LeafField[S, A, N]` | a leaf field tagged with the literal name `N`; `get`/`replace`/`/` |
| `Path[S, A]` | composed path (e.g. `user.address / Address.city`) |
| `FormFields[S]` | companion mixin providing `field(_.name)` |
| `AutoFields[S]` | companion mixin auto-deriving every field from `Schema` |

### Decoding

| Symbol | Purpose |
|---|---|
| `FieldDecoder[A]` | decode one raw string → `A` (leaf) |
| `FormDecoder[A]` | decode a `zio.http.Form` → `A` (structural) |
| `FormDecoder.derived` | auto-derive for case classes (or `derives FormDecoder`) |
| `DecodingError(field, message)` | structured decode error |

### Validation

| Symbol | Purpose |
|---|---|
| `Validator[A]` | pure validator; presets: `nonEmpty`, `minLength`, `maxLength`, `isEmail`, `isPhone`, `matches`, `min`, `max`, `requiredTrue`, `positive`, `isUrl`, `required`, `nonEmptySeq`, `custom`, `empty` |
| `ValidatorZIO[A]` | effectful validator (`++`, `contramap`, `map`, `option`, `compose`, `fromPure`) |
| `rule(field, validator)` | validate one field, keyed by its name |
| `whole(validator, path)` | whole-model / cross-field rule at a section path |
| `ruleEach(field, validation)` | validate every element of a `Seq` field (`docs.0.number`) |
| `Validation[S]` | a set of rules; `run(s): ZIO[Any, Nothing, Map[String, Seq[String]]]` |

### Rendering

| Symbol | Purpose |
|---|---|
| `RenderContext[A]` | path / old value / errors for the node being drawn |
| `drawField(field)(render)` | draw one field (`Drawn`) |
| `draw(fields...)(combine)` | assemble a `FormRenderer` + compile-time totality check |
| `FormRenderer[S, E]` | a rendered form (context → `E`) |
| `renderForm(form, oldValue, errors)` | render at the top level |

### Pipeline

| Symbol | Purpose |
|---|---|
| `decodeAndValidate(form)` | decode + validate → `ZIO[Any, Incomplete[A], A]` |
| `Incomplete[A]` | errors + old form, for re-rendering |

## Notes

- **Element-agnostic rendering**: the library knows nothing about HTML; you
  provide the widget palette (see `demo/src/Views.scala` for a Bootstrap 5
  example).
- **Effectful by default**: validation rules are `ZIO`; pure `Validator[A]`
  values auto-lift (`rule(field, Validator.nonEmpty)` just works).
- **Error messages** are in Russian; error keys are dot-notation paths
  (`address.city`, `docs.0.typ`).
- **Run the demo**: `./mill demo.run` (Bootstrap 5 form on port 8080).

## Modules

- `form4s` — the library (`io.github.npcrus::form4s`).
- `demo` — a runnable zio-http + Bootstrap 5 demo app.
- `test` — utest unit tests + Playwright E2E tests.

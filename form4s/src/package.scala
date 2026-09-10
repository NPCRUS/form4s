/** # form4s — type-safe web forms for Scala 3
  *
  * A small, optics-driven library for rendering, decoding, and validating web
  * forms. It splits the form problem into three orthogonal concerns that all
  * share a single source of field identity — the [[LeafField]] optic — so field
  * names are never hardcoded and stay in sync across rendering, decoding, and
  * validation automatically.
  *
  * ## The three concerns
  *
  *   - **Optics** (the shared core): [[Field]], [[LeafField]], [[Path]]. Obtain
  *     them with [[FormFields.field]] (one `val` per field) or, more concisely,
  *     with [[AutoFields]] (auto-derive all fields from a `Schema`).
  *   - **Decoding**: [[FieldDecoder]] (a single raw string → value) and
  *     [[FormDecoder]] (a `zio.http.Form` → typed value). Case classes are
  *     derived automatically with [[FormDecoder.derived]].
  *   - **Validation**: [[Validator]] (pure leaf), [[ValidatorZIO]] (effectful
  *     leaf), and the rule layer — [[rule]], [[whole]], [[ruleEach]] —
  *     assembled into a [[Validation]]. Rules are effectful (`ZIO`); pure
  *     validators auto-lift via [[Validator.toZIO]], so there is no ceremony.
  *   - **Rendering**: [[drawField]] + [[draw]] build a [[FormRenderer]];
  *     [[draw]] additionally enforces at compile time that every field is drawn
  *     exactly once. The library is element-agnostic — you pick the output type
  *     `E` (e.g. `List[zio.http.template2.Dom]`) and supply the fold that
  *     combines rendered pieces.
  *   - **Pipeline**: [[decodeAndValidate]] ties decode + validate together,
  *     failing with [[Incomplete]] so you can re-render via [[renderForm]].
  *
  * ## Quick start
  *
  * {{{
  * import zio.blocks.schema.Schema
  * import zio.http.template2.*
  *
  * case class Registration(username: String, age: Option[Int])
  * object Registration extends AutoFields[Registration]:
  *   given Schema[Registration] = Schema.derived
  *
  * given FormDecoder[Registration] = FormDecoder.derived
  *
  * given Validation[Registration] = Validation(
  *   rule(Registration.username, Validator.nonEmpty),
  *   rule(Registration.age, (Validator.min(1) ++ Validator.max(150)).option)
  * )
  *
  * val form: FormRenderer[Registration, List[Dom]] =
  *   draw(
  *     drawField(Registration.username)(ctx => List[Dom](input(name := ctx.path))),
  *     drawField(Registration.age)(ctx => List[Dom](input(name := ctx.path)))
  *   )(_ ++ _)
  *
  * // Initial render:    renderForm(form, None, Map.empty)
  * // After a bad submit: renderForm(form, incomplete.oldForm, incomplete.errors)
  * // On submit:         decodeAndValidate[Registration](formData)
  * }}}
  */
package form4s

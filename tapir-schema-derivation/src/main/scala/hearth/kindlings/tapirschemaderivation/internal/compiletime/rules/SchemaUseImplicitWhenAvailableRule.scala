package hearth.kindlings.tapirschemaderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import hearth.kindlings.tapirschemaderivation.KindlingsSchema
import sttp.tapir.Schema

trait SchemaUseImplicitWhenAvailableRuleImpl {
  this: SchemaMacrosImpl & MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>

  object SchemaUseImplicitWhenAvailableRule extends SchemaDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] =
      Type.of[KindlingsSchema.type].unsortedMethods.collect {
        case method if method.isImplicit => method.asUntyped
      } ++ Type.of[Schema.type].unsortedMethods.collect {
        // For tapir's own Schema companion, only ignore the auto-derivation method,
        // not built-in schemas for primitive types (schemaForString, schemaForInt, etc.).
        case method if method.name == "derivedSchema" => method.asUntyped
      }

    def apply[A: SchemaCtx]: MIO[Rule.Applicability[Expr[Schema[A]]]] =
      Log.info(s"Attempting to use implicit Schema for ${Type[A].prettyPrint}") >> {
        // Skip summoning for String-keyed Map types -- Tapir provides a built-in Schema[Map[String, V]]
        // but we need structural derivation (e.g. to respect the mapsAreArrays JSON config). Maps keyed
        // by anything else (e.g. an opaque/newtype key) have no such built-in schema to avoid, so let a
        // user-supplied Schema[Map[K, V]] win via the normal implicit search below.
        val isStringKeyedMapType: Boolean = Type[A] match {
          case IsMap(isMap) =>
            implicit val stringT: Type[String] = TsTypes.StringType
            isMap.value.Key <:< Type[String]
          case _ => false
        }
        if (isStringKeyedMapType) {
          Log.info(s"String-keyed map type detected, skipping summoning for ${Type[A].prettyPrint}") >>
            MIO.pure(Rule.yielded(s"String-keyed map type ${Type[A].prettyPrint} requires structural derivation"))
        } else if (sctx.derivedType.exists(_.Underlying =:= Type[A])) {
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search"))
        } else {
          implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
          Type[Schema[A]].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(expr) =>
              Log.info(s"Using summoned implicit Schema for ${Type[A].prettyPrint}") >>
                setCachedAndGet[A](sctx.cache, expr).map(Rule.matched)
            case Left(_) =>
              MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have an implicit Schema"))
          }
        }
      }
  }
}

package hearth.kindlings.tapirschemaderivation

import hearth.MacroSuite
import sttp.tapir.{Schema, SchemaType}

// `UserId` is opaque outside this object, so the test below can only pass if
// KindlingsSchema.derived lets a user-supplied Schema[Map[UserId, Int]] through
// for a Map keyed by a non-String opaque type, rather than requiring the key to
// be a String structurally. An opaque type defined directly alongside its use
// site (same object/package scope) would be dealiased away by the compiler and
// wouldn't exercise this at all.
object OpaqueKeyExample {
  opaque type UserId = String
  object UserId {
    def apply(value: String): UserId = value
    def raw(id: UserId): String = id
  }
}

case class WithOpaqueMapKey(counts: Map[OpaqueKeyExample.UserId, Int])

final class OpaqueMapKeySpec extends MacroSuite {

  group("KindlingsSchema.derived") {
    test("map field keyed by an opaque type uses the given custom Schema") {
      implicit val userIdMapSchema: Schema[Map[OpaqueKeyExample.UserId, Int]] =
        Schema.schemaForMap[OpaqueKeyExample.UserId, Int](OpaqueKeyExample.UserId.raw)

      val schema = KindlingsSchema.derived[WithOpaqueMapKey].schema
      schema.schemaType match {
        case p: SchemaType.SProduct[WithOpaqueMapKey] =>
          val fieldNames = p.fields.map(_.name.name)
          assertEquals(fieldNames, List("counts"))
        case other =>
          fail(s"Expected SProduct, got: $other")
      }
    }
  }
}

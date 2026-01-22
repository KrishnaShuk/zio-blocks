package zio.blocks.schema.json

import zio.blocks.schema._
import zio.test._

import zio.blocks.schema.json.Json._
import zio.blocks.schema.json.JsonInterpolators._

object JsonAdvancedSpec extends SchemaBaseSpec {

  def spec: Spec[TestEnvironment, Any] = suite("JsonAdvancedSpec")(
    suite("Merging")(
      test("merge objects (Auto/Deep)") {
        val json1 = json"""{"a": 1, "b": {"x": 10}}"""
        val json2 = json"""{"b": {"y": 20}, "c": 3}"""
        val expected = json"""{"a": 1, "b": {"x": 10, "y": 20}, "c": 3}"""
        assertTrue(json1.merge(json2) == expected)
      },
      test("merge arrays (Concat)") {
        val json1 = json"""[1, 2]"""
        val json2 = json"""[3, 4]"""
        val expected = json"""[1, 2, 3, 4]"""
        assertTrue(json1.merge(json2) == expected)
      },
      test("merge shallow") {
        val json1 = json"""{"a": 1, "b": {"x": 10}}"""
        val json2 = json"""{"b": {"y": 20}, "c": 3}"""
        // Shallow merge replaces "b" entirely
        val expected = json"""{"a": 1, "b": {"y": 20}, "c": 3}"""
        assertTrue(json1.merge(json2, MergeStrategy.Shallow) == expected)
      },
      test("merge replace") {
        val json1 = json"""{"a": 1}"""
        val json2 = json"""{"b": 2}"""
        assertTrue(json1.merge(json2, MergeStrategy.Replace) == json2)
      }
    ),
    suite("Transformation")(
      test("transformUp (increment numbers)") {
        val json = json"""{"a": 1, "b": [2, 3]}"""
        val result = json.transformUp { (_, v) =>
          v match {
            case Json.Number(n) => Json.Number((BigDecimal(n) + 1).toString)
            case other          => other
          }
        }
        val expected = json"""{"a": 2, "b": [3, 4]}"""
        assertTrue(result == expected)
      },
      test("transformKeys (prefix)") {
        val json = json"""{"a": 1, "b": {"c": 2}}"""
        val result = json.transformKeys { (_, k) => "x_" + k }
        val expected = json"""{"x_a": 1, "x_b": {"x_c": 2}}"""
        assertTrue(result == expected)
      }
    ),
    suite("Filtering")(
      test("filterNot (remove nulls)") {
        val json = json"""{"a": 1, "b": null, "c": [null, 2]}"""
        val result = json.filterNot { (_, v) => v.isNull }
        val expected = json"""{"a": 1, "c": [2]}"""
        assertTrue(result == expected)
      },
      test("filter (keep numbers)") {
        val json = json"""{"a": 1, "b": "foo", "c": [2, "bar"]}"""
        val result = json.filter { (_, v) => v.isNumber || v.isArray || v.isObject }
        val expected = json"""{"a": 1, "c": [2]}"""
        assertTrue(result == expected)
      }
    ),
    suite("Normalization")(
      test("sortKeys") {
        val json = json"""{"b": 2, "a": 1, "c": {"y": 2, "x": 1}}"""
        val result = json.sortKeys
        // Note: json"" interpolator might already sort keys depending on implementation,
        // but explicit construction ensures order.
        val expected = Json.Object(Vector(
          "a" -> Json.Number("1"),
          "b" -> Json.Number("2"),
          "c" -> Json.Object(Vector(
            "x" -> Json.Number("1"),
            "y" -> Json.Number("2")
          ))
        ))
        assertTrue(result == expected)
      },
      test("dropNulls") {
        val json = json"""{"a": 1, "b": null}"""
        assertTrue(json.dropNulls == json"""{"a": 1}""")
      },
      test("dropEmpty") {
        val json = json"""{"a": 1, "b": {}, "c": []}"""
        assertTrue(json.dropEmpty == json"""{"a": 1}""")
      }
    ),
    suite("Folding")(
      test("foldDown (collect keys)") {
        val json = json"""{"a": 1, "b": {"c": 2}}"""
        val keys = json.foldDown(List.empty[java.lang.String]) { (path, _, acc) =>
          path.nodes.lastOption match {
            case Some(DynamicOptic.Node.Field(name)) => acc :+ name
            case _ => acc
          }
        }
        assertTrue(keys.sorted == List("a", "b", "c"))
      }
    ),
    suite("Querying")(
      test("query (find all numbers)") {
        val json = json"""{"a": 1, "b": {"c": 2}, "d": [3, "foo"]}"""
        val result = json.query((_, v) => v.isNumber)
        val values = result.toArray.getOrElse(Json.Null)
        assertTrue(values == json"""[1, 2, 3]""")
      }
    ),
    suite("KV Interop")(
      test("toKV -> fromKV roundtrip") {
        val json = json"""{"a": 1, "b": {"c": 2}, "d": [3, 4]}"""
        val kvs = json.toKV
        val result = Json.fromKV(kvs)
        assertTrue(result == Right(json))
      }
    ),
    suite("OrFail Variants")(
      test("modifyOrFail success") {
        val json = json"""{"a": 1}"""
        val result = json.modifyOrFail(p"a", { case Json.Number(n) => Json.Number((BigDecimal(n) + 1).toString) })
        assertTrue(result == Right(json"""{"a": 2}"""))
      },
      test("modifyOrFail failure (path not found)") {
        val json = json"""{"a": 1}"""
        val result = json.modifyOrFail(p"b", { case x => x })
        assertTrue(result.isLeft)
      },
      test("insertOrFail") {
        val json = json"""{"a": 1}"""
        val result = json.insertOrFail(p"b", Json.Number("2"))
        assertTrue(result == Right(json"""{"a": 1, "b": 2}"""))
      }
    )
  )
}

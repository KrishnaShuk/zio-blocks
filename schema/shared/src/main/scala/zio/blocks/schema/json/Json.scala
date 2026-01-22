package zio.blocks.schema.json

import zio.blocks.schema.{DynamicOptic, DynamicValue, PrimitiveValue}
import zio.blocks.schema.DynamicOptic.Node
import scala.math.Ordering
import zio.blocks.schema.json.{JsonBinaryCodec, JsonReader, JsonWriter}

/**
 * Represents a JSON value.
 *
 * The JSON data model consists of:
 *   - '''Objects''': Unordered collections of key-value pairs
 *   - '''Arrays''': Ordered sequences of values
 *   - '''Strings''': Unicode text
 *   - '''Numbers''': Numeric values (stored as strings for precision)
 *   - '''Booleans''': `true` or `false`
 *   - '''Null''': The null value
 */
sealed trait Json { self =>

  /**
   * Type index for ordering: Null=0, Boolean=1, Number=2, String=3, Array=4,
   * Object=5
   */
  def typeIndex: Int

  /**
   * Compares this JSON to another for ordering.
   */
  def compare(that: Json): Int

  // ===========================================================================
  // Type Testing
  // ===========================================================================

  /**
   * Returns `true` if this is a JSON object.
   */
  def isObject: Boolean = false

  /**
   * Returns `true` if this is a JSON array.
   */
  def isArray: Boolean = false

  /**
   * Returns `true` if this is a JSON string.
   */
  def isString: Boolean = false

  /**
   * Returns `true` if this is a JSON number.
   */
  def isNumber: Boolean = false

  /**
   * Returns `true` if this is a JSON boolean.
   */
  def isBoolean: Boolean = false

  /**
   * Returns `true` if this is JSON null.
   */
  def isNull: Boolean = false

  // ===========================================================================
  // Type Filtering (returns JsonSelection)
  // ===========================================================================

  /**
   * Returns a [[JsonSelection]] containing this value if it is an object,
   * otherwise an empty selection.
   */
  def asObject: JsonSelection = if (isObject) JsonSelection(self) else JsonSelection.empty

  /**
   * Returns a [[JsonSelection]] containing this value if it is an array,
   * otherwise an empty selection.
   */
  def asArray: JsonSelection = if (isArray) JsonSelection(self) else JsonSelection.empty

  /**
   * Returns a [[JsonSelection]] containing this value if it is a string,
   * otherwise an empty selection.
   */
  def asString: JsonSelection = if (isString) JsonSelection(self) else JsonSelection.empty

  /**
   * Returns a [[JsonSelection]] containing this value if it is a number,
   * otherwise an empty selection.
   */
  def asNumber: JsonSelection = if (isNumber) JsonSelection(self) else JsonSelection.empty

  /**
   * Returns a [[JsonSelection]] containing this value if it is a boolean,
   * otherwise an empty selection.
   */
  def asBoolean: JsonSelection = if (isBoolean) JsonSelection(self) else JsonSelection.empty

  /**
   * Returns a [[JsonSelection]] containing this value if it is null,
   * otherwise an empty selection.
   */
  def asNull: JsonSelection = if (isNull) JsonSelection(self) else JsonSelection.empty

  // ===========================================================================
  // Direct Accessors
  // ===========================================================================

  /**
   * If this is an object, returns its fields as key-value pairs. Otherwise
   * returns an empty sequence.
   */
  def fields: Seq[(String, Json)] = Seq.empty

  /**
   * If this is an array, returns its elements. Otherwise returns an empty
   * sequence.
   */
  def elements: Seq[Json] = Seq.empty

  /**
   * If this is a string, returns its value. Otherwise returns `None`.
   */
  def stringValue: Option[String] = None

  /**
   * If this is a number, returns its string representation. Otherwise returns
   * `None`.
   */
  def numberValue: Option[String] = None

  /**
   * If this is a boolean, returns its value. Otherwise returns `None`.
   */
  def booleanValue: Option[scala.Boolean] = None

  // ===========================================================================
  // Navigation
  // ===========================================================================

  /**
   * Selects values at the given index if this is an array.
   */
  def apply(index: Int): JsonSelection = self match {
    case Json.Array(elems) if index >= 0 && index < elems.length =>
      JsonSelection(Right(Vector(elems(index))))
    case _ =>
      JsonSelection.empty
  }

  /**
   * Selects values with the given key if this is an object.
   */
  def apply(key: String): JsonSelection = self match {
    case Json.Object(fields) =>
      val matches = fields.collect { case (k, v) if k == key => v }
      JsonSelection(Right(matches))
    case _ =>
      JsonSelection.empty
  }

  /**
   * Navigates using a DynamicOptic path.
   */
  def get(path: DynamicOptic): JsonSelection =
    path.nodes.foldLeft(JsonSelection(self)) { (acc, node) =>
      acc.flatMap { json =>
        (json, node) match {
          case (Json.Object(fields), Node.Field(name)) =>
            val matches = fields.collect { case (k, v) if k == name => v }
            JsonSelection.fromVector(matches)
          case (Json.Array(elems), Node.AtIndex(index)) =>
            if (index >= 0 && index < elems.length) JsonSelection(elems(index))
            else JsonSelection.empty
          case _ => JsonSelection.empty
        }
      }
    }

  /**
   * Modifies the value at the given path.
   */
  def modify(path: DynamicOptic, f: Json => Json): Json =
    if (path.nodes.isEmpty) f(self)
    else {
      val head = path.nodes.head
      val tail = new DynamicOptic(path.nodes.tail)
      (self, head) match {
        case (Json.Object(fields), Node.Field(name)) =>
          val newFields = fields.map {
            case (k, v) if k == name => (k, v.modify(tail, f))
            case other               => other
          }
          Json.Object(newFields)
        case (Json.Array(elems), Node.AtIndex(index)) =>
          if (index >= 0 && index < elems.length) {
            val newElems = elems.updated(index, elems(index).modify(tail, f))
            Json.Array(newElems)
          } else self
        case _ => self
      }
    }

  /**
   * Sets the value at the given path.
   */
  def set(path: DynamicOptic, value: Json): Json = modify(path, _ => value)

  /**
   * Deletes the value at the given path.
   */
  def delete(path: DynamicOptic): Json =
    if (path.nodes.isEmpty) self
    else {
      val parentPath = new DynamicOptic(path.nodes.init)
      val targetNode = path.nodes.last
      modify(parentPath, _.deleteNode(targetNode))
    }

  private def deleteNode(node: DynamicOptic.Node): Json = (self, node) match {
    case (Json.Object(fields), Node.Field(name)) =>
      Json.Object(fields.filterNot(_._1 == name))
    case (Json.Array(elems), Node.AtIndex(index)) =>
      if (index >= 0 && index < elems.length) Json.Array(elems.patch(index, Vector.empty, 1))
      else self
    case _ => self
  }

  // ===========================================================================
  // Merging
  // ===========================================================================

  /**
   * Merges this JSON with another using the specified strategy.
   *
   * {{{
   * val merged = json1.merge(json2, MergeStrategy.Deep)
   * }}}
   *
   * @param other
   *   The JSON to merge with
   * @param strategy
   *   The merge strategy (default: [[MergeStrategy.Auto]])
   * @return
   *   The merged JSON
   */
  def merge(other: Json, strategy: MergeStrategy = MergeStrategy.Auto): Json =
    mergeInternal(other, strategy, DynamicOptic.root)

  private def mergeInternal(other: Json, strategy: MergeStrategy, path: DynamicOptic): Json =
    strategy match {
      case MergeStrategy.Replace => other
      case MergeStrategy.Shallow => (self, other) match {
        case (Json.Object(flds1), Json.Object(flds2)) =>
          val map1  = flds1.toMap
          val map2  = flds2.toMap
          val merged = (map1 ++ map2).toVector
          Json.Object(merged)
        case _ => other
      }
      case MergeStrategy.Concat => (self, other) match {
        case (Json.Array(e1), Json.Array(e2)) => Json.Array(e1 ++ e2)
        case _                                => other
      }
      case MergeStrategy.Deep => (self, other) match {
        case (Json.Object(flds1), Json.Object(flds2)) =>
          val map1 = flds1.toMap
          val map2 = flds2.toMap
          val allKeys = (map1.keys ++ map2.keys).toVector.distinct
          val merged = allKeys.map { k =>
            (map1.get(k), map2.get(k)) match {
              case (Some(v1), Some(v2)) => (k, v1.mergeInternal(v2, strategy, path.field(k)))
              case (Some(v1), None)     => (k, v1)
              case (None, Some(v2))     => (k, v2)
              case (None, None)         => (k, Json.Null) // should not happen
            }
          }
          Json.Object(merged)
        case (Json.Array(e1), Json.Array(e2)) => Json.Array(e1 ++ e2)
        case _ => other
      }
      case MergeStrategy.Auto => (self, other) match {
        case (Json.Object(flds1), Json.Object(flds2)) =>
          val map1 = flds1.toMap
          val map2 = flds2.toMap
          val allKeys = (map1.keys ++ map2.keys).toVector.distinct
          val merged = allKeys.map { k =>
            (map1.get(k), map2.get(k)) match {
              case (Some(v1), Some(v2)) => (k, v1.mergeInternal(v2, MergeStrategy.Auto, path.field(k)))
              case (Some(v1), None)     => (k, v1)
              case (None, Some(v2))     => (k, v2)
              case (None, None)         => (k, Json.Null)
            }
          }
          Json.Object(merged)
        case (Json.Array(e1), Json.Array(e2)) => Json.Array(e1 ++ e2)
        case _ => other
      }
      case MergeStrategy.Custom(f) => f(path, self, other)
    }

  // ===========================================================================
  // Transformation
  // ===========================================================================

  /**
   * Transforms all values in this JSON bottom-up (children before parents).
   *
   * @param f The transformation function receiving path and value
   * @return The transformed JSON
   */
  def transformUp(f: (DynamicOptic, Json) => Json): Json =
    transformUpInternal(f, DynamicOptic.root)

  private def transformUpInternal(f: (DynamicOptic, Json) => Json, path: DynamicOptic): Json = {
    val transformed = self match {
      case Json.Object(flds) =>
        Json.Object(flds.map { case (k, v) =>
          (k, v.transformUpInternal(f, path.field(k)))
        })
      case Json.Array(elems) =>
        Json.Array(elems.zipWithIndex.map { case (v, i) =>
          v.transformUpInternal(f, path.at(i))
        })
      case other => other
    }
    f(path, transformed)
  }

  /**
   * Transforms all values in this JSON top-down (parents before children).
   *
   * @param f The transformation function receiving path and value
   * @return The transformed JSON
   */
  def transformDown(f: (DynamicOptic, Json) => Json): Json =
    transformDownInternal(f, DynamicOptic.root)

  private def transformDownInternal(f: (DynamicOptic, Json) => Json, path: DynamicOptic): Json = {
    val transformed = f(path, self)
    transformed match {
      case Json.Object(flds) =>
        Json.Object(flds.map { case (k, v) =>
          (k, v.transformDownInternal(f, path.field(k)))
        })
      case Json.Array(elems) =>
        Json.Array(elems.zipWithIndex.map { case (v, i) =>
          v.transformDownInternal(f, path.at(i))
        })
      case other => other
    }
  }

  /**
   * Transforms all object keys in this JSON.
   *
   * @param f The key transformation function receiving path and key
   * @return The transformed JSON
   */
  def transformKeys(f: (DynamicOptic, String) => String): Json =
    transformKeysInternal(f, DynamicOptic.root)

  private def transformKeysInternal(f: (DynamicOptic, String) => String, path: DynamicOptic): Json =
    self match {
      case Json.Object(flds) =>
        Json.Object(flds.map { case (k, v) =>
          val newKey = f(path, k)
          (newKey, v.transformKeysInternal(f, path.field(k)))
        })
      case Json.Array(elems) =>
        Json.Array(elems.zipWithIndex.map { case (v, i) =>
          v.transformKeysInternal(f, path.at(i))
        })
      case other => other
    }

  // ===========================================================================
  // Filtering
  // ===========================================================================

  /**
   * Removes entries matching the predicate.
   *
   * For objects, removes matching key-value pairs.
   * For arrays, removes matching elements.
   *
   * @param p The predicate receiving path and value
   * @return The filtered JSON
   */
  def filterNot(p: (DynamicOptic, Json) => scala.Boolean): Json =
    filterNotInternal(p, DynamicOptic.root)

  private def filterNotInternal(p: (DynamicOptic, Json) => scala.Boolean, path: DynamicOptic): Json =
    self match {
      case Json.Object(flds) =>
        Json.Object(flds.flatMap { case (k, v) =>
          val childPath = path.field(k)
          if (p(childPath, v)) None
          else Some((k, v.filterNotInternal(p, childPath)))
        })
      case Json.Array(elems) =>
        Json.Array(elems.zipWithIndex.flatMap { case (v, i) =>
          val childPath = path.at(i)
          if (p(childPath, v)) None
          else Some(v.filterNotInternal(p, childPath))
        })
      case other => other
    }

  /**
   * Keeps only entries matching the predicate.
   *
   * @param p The predicate receiving path and value
   * @return The filtered JSON
   */
  def filter(p: (DynamicOptic, Json) => scala.Boolean): Json =
    filterNot((path, json) => !p(path, json))

  // ===========================================================================
  // Folding
  // ===========================================================================

  /**
   * Folds over this JSON top-down (parents before children).
   *
   * @param z The initial accumulator value
   * @param f The fold function receiving path, value, and accumulator
   * @tparam B The accumulator type
   * @return The final accumulated value
   */
  def foldDown[B](z: B)(f: (DynamicOptic, Json, B) => B): B =
    foldDownInternal(z, f, DynamicOptic.root)

  private def foldDownInternal[B](z: B, f: (DynamicOptic, Json, B) => B, path: DynamicOptic): B = {
    val acc = f(path, self, z)
    self match {
      case Json.Object(flds) =>
        flds.foldLeft(acc) { case (a, (k, v)) =>
          v.foldDownInternal(a, f, path.field(k))
        }
      case Json.Array(elems) =>
        elems.zipWithIndex.foldLeft(acc) { case (a, (v, i)) =>
          v.foldDownInternal(a, f, path.at(i))
        }
      case _ => acc
    }
  }

  /**
   * Folds over this JSON bottom-up (children before parents).
   *
   * @param z The initial accumulator value
   * @param f The fold function receiving path, value, and accumulator
   * @tparam B The accumulator type
   * @return The final accumulated value
   */
  def foldUp[B](z: B)(f: (DynamicOptic, Json, B) => B): B =
    foldUpInternal(z, f, DynamicOptic.root)

  private def foldUpInternal[B](z: B, f: (DynamicOptic, Json, B) => B, path: DynamicOptic): B = {
    val childAcc = self match {
      case Json.Object(flds) =>
        flds.foldLeft(z) { case (a, (k, v)) =>
          v.foldUpInternal(a, f, path.field(k))
        }
      case Json.Array(elems) =>
        elems.zipWithIndex.foldLeft(z) { case (a, (v, i)) =>
          v.foldUpInternal(a, f, path.at(i))
        }
      case _ => z
    }
    f(path, self, childAcc)
  }

  /**
   * Folds over this JSON top-down, allowing the fold function to fail.
   *
   * Short-circuits on first failure.
   */
  def foldDownOrFail[B](z: B)(f: (DynamicOptic, Json, B) => Either[JsonError, B]): Either[JsonError, B] =
    foldDownOrFailInternal(z, f, DynamicOptic.root)

  private def foldDownOrFailInternal[B](
    z: B,
    f: (DynamicOptic, Json, B) => Either[JsonError, B],
    path: DynamicOptic
  ): Either[JsonError, B] =
    f(path, self, z).flatMap { acc =>
      self match {
        case Json.Object(flds) =>
          flds.foldLeft[Either[JsonError, B]](Right(acc)) { case (eAcc, (k, v)) =>
            eAcc.flatMap(a => v.foldDownOrFailInternal(a, f, path.field(k)))
          }
        case Json.Array(elems) =>
          elems.zipWithIndex.foldLeft[Either[JsonError, B]](Right(acc)) { case (eAcc, (v, i)) =>
            eAcc.flatMap(a => v.foldDownOrFailInternal(a, f, path.at(i)))
          }
        case _ => Right(acc)
      }
    }

  /**
   * Folds over this JSON bottom-up, allowing the fold function to fail.
   *
   * Short-circuits on first failure.
   */
  def foldUpOrFail[B](z: B)(f: (DynamicOptic, Json, B) => Either[JsonError, B]): Either[JsonError, B] =
    foldUpOrFailInternal(z, f, DynamicOptic.root)

  private def foldUpOrFailInternal[B](
    z: B,
    f: (DynamicOptic, Json, B) => Either[JsonError, B],
    path: DynamicOptic
  ): Either[JsonError, B] = {
    val childResult: Either[JsonError, B] = self match {
      case Json.Object(flds) =>
        flds.foldLeft[Either[JsonError, B]](Right(z)) { case (eAcc, (k, v)) =>
          eAcc.flatMap(a => v.foldUpOrFailInternal(a, f, path.field(k)))
        }
      case Json.Array(elems) =>
        elems.zipWithIndex.foldLeft[Either[JsonError, B]](Right(z)) { case (eAcc, (v, i)) =>
          eAcc.flatMap(a => v.foldUpOrFailInternal(a, f, path.at(i)))
        }
      case _ => Right(z)
    }
    childResult.flatMap(acc => f(path, self, acc))
  }

  // ===========================================================================
  // Querying
  // ===========================================================================

  /**
   * Selects all values matching the predicate.
   *
   * @param p The predicate receiving path and value
   * @return A [[JsonSelection]] containing matching values
   */
  def query(p: (DynamicOptic, Json) => scala.Boolean): JsonSelection =
    queryInternal(p, DynamicOptic.root)

  private def queryInternal(p: (DynamicOptic, Json) => scala.Boolean, path: DynamicOptic): JsonSelection = {
    val selfMatches  = if (p(path, self)) Vector(self) else Vector.empty
    val childMatches = self match {
      case Json.Object(flds) =>
        flds.flatMap { case (k, v) =>
          v.queryInternal(p, path.field(k)).toEither.getOrElse(Vector.empty)
        }
      case Json.Array(elems) =>
        elems.zipWithIndex.flatMap { case (v, i) =>
          v.queryInternal(p, path.at(i)).toEither.getOrElse(Vector.empty)
        }
      case _ => Vector.empty
    }
    JsonSelection.fromVector(selfMatches ++ childMatches)
  }

  // ===========================================================================
  // Projection / Partitioning
  // ===========================================================================

  /**
   * Projects this JSON to include only the specified paths.
   *
   * Paths that don't exist are ignored. Structure is preserved.
   *
   * @param paths The paths to include
   * @return A new JSON containing only the specified paths
   */
  def project(paths: DynamicOptic*): Json = {
    if (paths.isEmpty) Json.Null
    else {
      paths.foldLeft[Option[Json]](None) { (acc, path) =>
        val extracted = get(path).first.toOption
        extracted match {
          case Some(value) =>
            val built = buildPath(path, value)
            acc match {
              case Some(existing) => Some(existing.merge(built, MergeStrategy.Deep))
              case None           => Some(built)
            }
          case None => acc
        }
      }.getOrElse(Json.Null)
    }
  }

  private def buildPath(path: DynamicOptic, value: Json): Json = {
    path.nodes.foldRight(value) { (node, acc) =>
      node match {
        case Node.Field(name)   => Json.Object(Vector((name, acc)))
        case Node.AtIndex(idx)  =>
          val arr = Vector.fill(idx)(Json.Null) :+ acc
          Json.Array(arr)
        case _ => acc
      }
    }
  }

  /**
   * Partitions this JSON into two based on a predicate.
   *
   * Returns a tuple where the first element contains entries satisfying
   * the predicate, and the second contains entries that don't.
   *
   * @param p The predicate receiving path and value
   * @return A tuple of (matching, non-matching) JSON values
   */
  def partition(p: (DynamicOptic, Json) => scala.Boolean): (Json, Json) =
    (filter(p), filterNot(p))

  // ===========================================================================
  // KV Representation
  // ===========================================================================

  /**
   * Flattens this JSON to a sequence of path-value pairs.
   *
   * Only leaf values (primitives, empty arrays, empty objects) are included.
   */
  def toKV: Seq[(DynamicOptic, Json)] = toKVInternal(DynamicOptic.root)

  private def toKVInternal(path: DynamicOptic): Seq[(DynamicOptic, Json)] = self match {
    case Json.Object(flds) if flds.nonEmpty =>
      flds.flatMap { case (k, v) => v.toKVInternal(path.field(k)) }
    case Json.Array(elems) if elems.nonEmpty =>
      elems.zipWithIndex.flatMap { case (v, i) => v.toKVInternal(path.at(i)) }
    case _ => Seq((path, self))
  }

  // ===========================================================================
  // Error-Returning Variants
  // ===========================================================================

  /**
   * Modifies values at the given path using a partial function.
   * Returns an error if the path is invalid or the partial function is not defined.
   */
  def modifyOrFail(path: DynamicOptic, pf: PartialFunction[Json, Json]): Either[JsonError, Json] =
    get(path).first match {
      case Right(target) if pf.isDefinedAt(target) =>
        Right(modify(path, pf))
      case Right(_) =>
        Left(JsonError(s"Partial function not defined for value at path $path", path))
      case Left(err) =>
        Left(JsonError.fromSchemaError(err))
    }

  /**
   * Sets the value at the given path, returning an error if the path is invalid.
   */
  def setOrFail(path: DynamicOptic, value: Json): Either[JsonError, Json] =
    if (path.nodes.isEmpty) Right(value)
    else {
      val parentPath = new DynamicOptic(path.nodes.init)
      get(parentPath).first match {
        case Right(_) => Right(set(path, value))
        case Left(err) => Left(JsonError.fromSchemaError(err))
      }
    }

  /**
   * Deletes values at the given path, returning an error if the path is invalid.
   */
  def deleteOrFail(path: DynamicOptic): Either[JsonError, Json] =
    if (path.nodes.isEmpty) Left(JsonError("Cannot delete root"))
    else {
      get(path).first match {
        case Right(_)  => Right(delete(path))
        case Left(err) => Left(JsonError.fromSchemaError(err))
      }
    }

  /**
   * Inserts a value at the given path.
   *
   * For arrays, inserts at the specified index, shifting subsequent elements.
   * For objects, adds or replaces the key.
   */
  def insert(path: DynamicOptic, value: Json): Json =
    if (path.nodes.isEmpty) value
    else {
      val parentPath = new DynamicOptic(path.nodes.init)
      val targetNode = path.nodes.last
      modify(parentPath, parent => (parent, targetNode) match {
        case (Json.Array(elems), Node.AtIndex(idx)) =>
          val insertIdx = math.min(math.max(0, idx), elems.length)
          Json.Array(elems.patch(insertIdx, Vector(value), 0))
        case (Json.Object(flds), Node.Field(name)) =>
          val filtered = flds.filterNot(_._1 == name)
          Json.Object(filtered :+ (name, value))
        case _ => parent
      })
    }

  /**
   * Inserts a value at the given path, returning an error if invalid.
   */
  def insertOrFail(path: DynamicOptic, value: Json): Either[JsonError, Json] =
    if (path.nodes.isEmpty) Right(value)
    else {
      val parentPath = new DynamicOptic(path.nodes.init)
      get(parentPath).first match {
        case Right(_)  => Right(insert(path, value))
        case Left(err) => Left(JsonError.fromSchemaError(err))
      }
    }

  /**
   * Decodes this JSON value to a value of type `A`.
   */
  def as[A](implicit decoder: JsonDecoder[A]): Either[JsonError, A] = decoder.decode(self)

  /**
   * Decodes this JSON value to a value of type `A`, throwing on failure.
   */
  def asUnsafe[A](implicit decoder: JsonDecoder[A]): A = as[A].fold(throw _, identity)

  // ===========================================================================
  // Normalization
  // ===========================================================================

  /**
   * Returns this JSON with all object keys sorted alphabetically (recursive).
   */
  def sortKeys: Json = self match {
    case Json.Object(flds) =>
      Json.Object(flds.map { case (k, v) => (k, v.sortKeys) }.sortBy(_._1))
    case Json.Array(elems) =>
      Json.Array(elems.map(_.sortKeys))
    case other =>
      other
  }

  /**
   * Returns this JSON with all null values removed from objects.
   */
  def dropNulls: Json = self match {
    case Json.Object(flds) =>
      Json.Object(flds.collect { case (k, v) if !v.isNull => (k, v.dropNulls) })
    case Json.Array(elems) =>
      Json.Array(elems.map(_.dropNulls))
    case other =>
      other
  }

  /**
   * Returns this JSON with empty objects and arrays removed.
   */
  def dropEmpty: Json = self match {
    case Json.Object(flds) =>
      val filtered = flds.flatMap { case (k, v) =>
        val dropped = v.dropEmpty
        dropped match {
          case Json.Object(f) if f.isEmpty => None
          case Json.Array(e) if e.isEmpty  => None
          case other                       => Some((k, other))
        }
      }
      Json.Object(filtered)
    case Json.Array(elems) =>
      val filtered = elems.map(_.dropEmpty).filter {
        case Json.Object(f) if f.isEmpty => false
        case Json.Array(e) if e.isEmpty  => false
        case _                           => true
      }
      Json.Array(filtered)
    case other =>
      other
  }

  // ===========================================================================
  // DynamicValue Conversion
  // ===========================================================================

  /**
   * Converts this JSON value to a [[DynamicValue]].
   */
  def toDynamicValue: DynamicValue = self match {
    case Json.Null =>
      DynamicValue.Primitive(PrimitiveValue.Unit)
    case Json.Boolean(b) =>
      DynamicValue.Primitive(PrimitiveValue.Boolean(b))
    case Json.Number(s) =>
      // Use BigDecimal to preserve precision
      try {
        DynamicValue.Primitive(PrimitiveValue.BigDecimal(BigDecimal(s)))
      } catch {
        case _: NumberFormatException =>
          DynamicValue.Primitive(PrimitiveValue.String(s))
      }
    case Json.String(s) =>
      DynamicValue.Primitive(PrimitiveValue.String(s))
    case Json.Array(elems) =>
      DynamicValue.Sequence(elems.map(_.toDynamicValue))
    case Json.Object(fields) =>
      DynamicValue.Record(fields.map { case (k, v) => (k, v.toDynamicValue) })
  }

  // ===========================================================================
  // Encoding (Instance Methods)
  // ===========================================================================

  /**
   * Encodes this JSON to a compact string (no extra whitespace).
   */
  def print: String = Json.encode(self)

  /**
   * Encodes this JSON to a string using the specified configuration.
   *
   * @param config Writer configuration (indentation, unicode escaping, etc.)
   */
  def print(config: WriterConfig): String = Json.encode(self, config)

  /**
   * Alias for [[print]].
   */
  def encode: String = print

  /**
   * Encodes this JSON to a string using the specified configuration.
   *
   * @param config Writer configuration
   */
  def encode(config: WriterConfig): String = print(config)

  /**
   * Encodes this JSON to a byte array (UTF-8).
   */
  def encodeToBytes: Array[Byte] = Json.encodeToBytes(self)

  /**
   * Encodes this JSON to a byte array (UTF-8) with configuration.
   *
   * @param config Writer configuration
   */
  def encodeToBytes(config: WriterConfig): Array[Byte] = Json.encodeToBytes(self, config)

  // ===========================================================================
  // Standard Methods
  // ===========================================================================

  override def toString: String = self match {
    case Json.Null         => "null"
    case Json.Boolean(v)   => v.toString
    case Json.Number(v)    => v
    case Json.String(v)    => "\"" + v + "\""
    case Json.Array(elems) => elems.mkString("[", ",", "]")
    case Json.Object(flds) => flds.map { case (k, v) => "\"" + k + "\":" + v }.mkString("{", ",", "}")
  }
}

object Json {

  /**
   * Encodes a value of type `A` to JSON.
   */
  def from[A](value: A)(implicit encoder: JsonEncoder[A]): Json = encoder.encode(value)

  // ===========================================================================
  // ADT Cases
  // ===========================================================================

  /**
   * The JSON null value.
   */
  case object Null extends Json {
    override def isNull: scala.Boolean = true
    override def typeIndex: Int        = 0

    override def compare(that: Json): Int = that match {
      case Null => 0
      case _    => -that.typeIndex
    }
  }

  /**
   * A JSON boolean.
   *
   * @param value
   *   The boolean value
   */
  final case class Boolean(value: scala.Boolean) extends Json {
    override def isBoolean: scala.Boolean            = true
    override def booleanValue: Option[scala.Boolean] = Some(value)
    override def typeIndex: Int                      = 1

    override def compare(that: Json): Int = that match {
      case Boolean(thatValue) => value.compare(thatValue)
      case _                  => 1 - that.typeIndex
    }
  }

  object Boolean {
    val True: Boolean  = Boolean(true)
    val False: Boolean = Boolean(false)
  }

  /**
   * A JSON number.
   *
   * Stored as a string to preserve exact representation (precision, trailing
   * zeros, etc.).
   *
   * @param value
   *   The number as a string (should be valid JSON number syntax)
   */
  final case class Number(value: java.lang.String) extends Json {
    override def isNumber: scala.Boolean               = true
    override def numberValue: Option[java.lang.String] = Some(value)
    override def typeIndex: Int                        = 2

    override def compare(that: Json): Int = that match {
      case Number(thatValue) => BigDecimal(value).compare(BigDecimal(thatValue))
      case _                 => 2 - that.typeIndex
    }

    /**
     * Converts to `Int`, truncating if necessary.
     */
    lazy val toInt: Int = toBigDecimal.toInt

    /**
     * Converts to `Long`, truncating if necessary.
     */
    lazy val toLong: Long = toBigDecimal.toLong

    /**
     * Converts to `Float`.
     */
    lazy val toFloat: Float = value.toFloat

    /**
     * Converts to `Double`.
     */
    lazy val toDouble: Double = value.toDouble

    /**
     * Converts to `BigInt`, truncating fractional part.
     */
    lazy val toBigInt: BigInt = toBigDecimal.toBigInt

    /**
     * Converts to `BigDecimal` (lossless).
     */
    lazy val toBigDecimal: BigDecimal = BigDecimal(value)
  }

  /**
   * A JSON string.
   *
   * @param value
   *   The string value (unescaped)
   */
  final case class String(value: java.lang.String) extends Json {
    override def isString: scala.Boolean               = true
    override def stringValue: Option[java.lang.String] = Some(value)
    override def typeIndex: Int                        = 3

    override def compare(that: Json): Int = that match {
      case String(thatValue) => value.compareTo(thatValue)
      case _                 => 3 - that.typeIndex
    }
  }

  /**
   * A JSON array: an ordered sequence of values.
   *
   * @param elements
   *   The array elements
   */
  final case class Array(elems: Vector[Json]) extends Json {
    override def isArray: scala.Boolean = true
    override def elements: Seq[Json]    = elems
    override def typeIndex: Int         = 4

    override def equals(that: Any): scala.Boolean = that match {
      case Array(thatElems) =>
        val len = elems.length
        if (len != thatElems.length) return false
        var idx = 0
        while (idx < len) {
          if (elems(idx) != thatElems(idx)) return false
          idx += 1
        }
        true
      case _ => false
    }

    override def hashCode: Int = elems.hashCode

    override def compare(that: Json): Int = that match {
      case Array(thatElems) =>
        val xs     = elems
        val ys     = thatElems
        val xLen   = xs.length
        val yLen   = ys.length
        val minLen = Math.min(xLen, yLen)
        var idx    = 0
        while (idx < minLen) {
          val cmp = xs(idx).compare(ys(idx))
          if (cmp != 0) return cmp
          idx += 1
        }
        xLen.compareTo(yLen)
      case _ => 4 - that.typeIndex
    }
  }

  object Array {

    /**
     * Creates an empty JSON array.
     */
    val empty: Array = Array(Vector.empty)

    /**
     * Creates a JSON array from elements.
     */
    def apply(elements: Json*): Array = Array(elements.toVector)
  }

  /**
   * A JSON object: an unordered collection of key-value pairs.
   *
   * Equality and comparison are order-independent: `{"a":1, "b":2}` equals
   * `{"b":2, "a":1}`.
   *
   * @param flds
   *   The key-value pairs. Keys should be unique; if duplicates are present,
   *   behavior of accessors is undefined.
   */
  final case class Object(flds: Vector[(java.lang.String, Json)]) extends Json {
    override def isObject: scala.Boolean               = true
    override def fields: Seq[(java.lang.String, Json)] = flds
    override def typeIndex: Int                        = 5

    /**
     * Cached sorted fields for order-independent comparison.
     */
    private lazy val sortedFields: Vector[(java.lang.String, Json)] = flds.sortBy(_._1)

    override def equals(that: Any): scala.Boolean = that match {
      case thatObj: Object =>
        if (flds.length != thatObj.flds.length) return false
        val xs  = sortedFields
        val ys  = thatObj.sortedFields
        val len = xs.length
        var idx = 0
        while (idx < len) {
          val kv1 = xs(idx)
          val kv2 = ys(idx)
          if (kv1._1 != kv2._1 || kv1._2 != kv2._2) return false
          idx += 1
        }
        true
      case _ => false
    }

    override def hashCode: Int = sortedFields.hashCode

    override def compare(that: Json): Int = that match {
      case thatObj: Object =>
        val xs     = sortedFields
        val ys     = thatObj.sortedFields
        val xLen   = xs.length
        val yLen   = ys.length
        val minLen = Math.min(xLen, yLen)
        var idx    = 0
        while (idx < minLen) {
          val kv1 = xs(idx)
          val kv2 = ys(idx)
          var cmp = kv1._1.compareTo(kv2._1)
          if (cmp != 0) return cmp
          cmp = kv1._2.compare(kv2._2)
          if (cmp != 0) return cmp
          idx += 1
        }
        xLen.compareTo(yLen)
      case _ => 5 - that.typeIndex
    }
  }

  object Object {

    /**
     * Creates an empty JSON object.
     */
    val empty: Object = Object(Vector.empty)

    /**
     * Creates a JSON object from key-value pairs.
     */
    def apply(fields: (java.lang.String, Json)*): Object = Object(fields.toVector)
  }

  // ===========================================================================
  // Convenience Constructors
  // ===========================================================================

  /**
   * Creates a JSON number from an `Int`.
   */
  def number(n: Int): Number = Number(n.toString)

  /**
   * Creates a JSON number from a `Long`.
   */
  def number(n: Long): Number = Number(n.toString)

  /**
   * Creates a JSON number from a `Float`.
   */
  def number(n: Float): Number = Number(n.toString)

  /**
   * Creates a JSON number from a `Double`.
   */
  def number(n: Double): Number = Number(n.toString)

  /**
   * Creates a JSON number from a `BigInt`.
   */
  def number(n: BigInt): Number = Number(n.toString)

  /**
   * Creates a JSON number from a `BigDecimal`.
   */
  def number(n: BigDecimal): Number = Number(n.toString)

  /**
   * Creates a JSON number from a `Short`.
   */
  def number(n: Short): Number = Number(n.toString)

  /**
   * Creates a JSON number from a `Byte`.
   */
  def number(n: Byte): Number = Number(n.toString)

  // ===========================================================================
  // DynamicValue Conversion
  // ===========================================================================

  /**
   * Converts a [[DynamicValue]] to a JSON value.
   */
  def fromDynamicValue(dv: DynamicValue): Json = dv match {
    case DynamicValue.Primitive(pv) =>
      pv match {
        case PrimitiveValue.Unit           => Json.Null
        case PrimitiveValue.Boolean(b)     => Json.Boolean(b)
        case PrimitiveValue.Byte(b)        => Json.Number(b.toString)
        case PrimitiveValue.Short(s)       => Json.Number(s.toString)
        case PrimitiveValue.Int(i)         => Json.Number(i.toString)
        case PrimitiveValue.Long(l)        => Json.Number(l.toString)
        case PrimitiveValue.Float(f)       => Json.Number(f.toString)
        case PrimitiveValue.Double(d)      => Json.Number(d.toString)
        case PrimitiveValue.BigInt(bi)     => Json.Number(bi.toString)
        case PrimitiveValue.BigDecimal(bd) => Json.Number(bd.toString)
        case PrimitiveValue.String(s)      => Json.String(s)
        case PrimitiveValue.Char(c)        => Json.String(c.toString)
        case other                         => Json.String(other.toString)

      }
    case DynamicValue.Record(fields) =>
      Json.Object(fields.map { case (k, v) => (k, fromDynamicValue(v)) })
    case DynamicValue.Sequence(elements) =>
      Json.Array(elements.map(fromDynamicValue))
    case DynamicValue.Map(entries) =>
      // Convert map entries to JSON object if keys are strings, otherwise to array of pairs
      val allKeysAreStrings = entries.forall {
        case (DynamicValue.Primitive(PrimitiveValue.String(_)), _) => true
        case _                                                     => false
      }
      if (allKeysAreStrings) {
        val fields = entries.map {
          case (DynamicValue.Primitive(PrimitiveValue.String(k)), v) => (k, fromDynamicValue(v))
          case _                                                     => throw new IllegalStateException("Unexpected non-string key")
        }
        Json.Object(fields)
      } else {
        // Represent as array of [key, value] pairs
        Json.Array(entries.map { case (k, v) =>
          Json.Array(Vector(fromDynamicValue(k), fromDynamicValue(v)))
        })
      }
    case DynamicValue.Variant(caseName, value) =>
      // Represent variant as object with single field
      Json.Object(Vector((caseName, fromDynamicValue(value))))
  }

  // ===========================================================================
  // Codec & Parsing
  // ===========================================================================

  implicit val codec: JsonBinaryCodec[Json] = new JsonBinaryCodec[Json] {
    def decodeValue(in: JsonReader, default: Json): Json = {
      val b = in.nextToken()
      if (b == '"') {
        in.rollbackToken()
        Json.String(in.readString(null))
      } else if (b == 'f' || b == 't') {
        in.rollbackToken()
        Json.Boolean(in.readBoolean())
      } else if (b >= '0' && b <= '9' || b == '-') {
        in.rollbackToken()
        val bd = in.readBigDecimal(null)
        Json.Number(bd.toString)
      } else if (b == '[') {
        if (in.isNextToken(']')) Json.Array(Vector.empty)
        else {
          in.rollbackToken()
          val builder = new scala.collection.immutable.VectorBuilder[Json]
          while ({
            builder.addOne(decodeValue(in, default))
            in.isNextToken(',')
          }) ()
          if (in.isCurrentToken(']')) Json.Array(builder.result())
          else in.arrayEndOrCommaError()
        }
      } else if (b == '{') {
        if (in.isNextToken('}')) Json.Object(Vector.empty)
        else {
          in.rollbackToken()
          val builder = new scala.collection.immutable.VectorBuilder[(java.lang.String, Json)]
          while ({
            builder.addOne((in.readKeyAsString(), decodeValue(in, default)))
            in.isNextToken(',')
          }) ()
          if (in.isCurrentToken('}')) Json.Object(builder.result())
          else in.objectEndOrCommaError()
        }
      } else {
        in.rollbackToken()
        in.readNullOrError(Json.Null, "expected JSON value")
      }
    }

    def encodeValue(x: Json, out: JsonWriter): Unit = x match {
      case Json.Null       => out.writeNull()
      case Json.Boolean(b) => out.writeVal(b)
      case Json.Number(s)  => out.writeVal(BigDecimal(s))
      case Json.String(s)  => out.writeVal(s)
      case Json.Array(xs)  =>
        out.writeArrayStart()
        xs.foreach(encodeValue(_, out))
        out.writeArrayEnd()
      case Json.Object(xs) =>
        out.writeObjectStart()
        xs.foreach { case (k, v) =>
          out.writeKey(k)
          encodeValue(v, out)
        }
        out.writeObjectEnd()
    }
  }

  /**
   * Parses a string into a JSON value.
   */
  def parse(s: java.lang.String): Either[JsonError, Json] =
    codec.decode(s).left.map(e => JsonError(e.getMessage))

  /**
   * Parses a byte array (UTF-8) into a JSON value.
   */
  def parse(bytes: scala.Array[Byte]): Either[JsonError, Json] =
    codec.decode(bytes).left.map(e => JsonError(e.getMessage))

  /**
   * Parses a string into a JSON value, throwing on failure.
   */
  def parseUnsafe(s: java.lang.String): Json =
    parse(s).fold(throw _, identity)

  /**
   * Alias for [[parseUnsafe]].
   */
  def decodeUnsafe(s: java.lang.String): Json = parseUnsafe(s)

  /**
   * Encodes a JSON value into a string.
   */
  def encode(json: Json): java.lang.String = codec.encodeToString(json)

  /**
   * Encodes a JSON value into a string with configuration.
   */
  def encode(json: Json, config: WriterConfig): java.lang.String = codec.encodeToString(json, config)

  /**
   * Encodes a JSON value into a byte array (UTF-8).
   */
  def encodeToBytes(json: Json): scala.Array[Byte] = codec.encode(json)

  /**
   * Encodes a JSON value into a byte array (UTF-8) with configuration.
   */
  def encodeToBytes(json: Json, config: WriterConfig): scala.Array[Byte] = codec.encode(json, config)

  // ===========================================================================
  // KV Interop
  // ===========================================================================

  /**
   * Assembles JSON from a sequence of path-value pairs.
   *
   * @param kvs The path-value pairs
   * @return Either an error (for conflicting paths) or the assembled JSON
   */
  def fromKV(kvs: Seq[(DynamicOptic, Json)]): Either[JsonError, Json] = {
    if (kvs.isEmpty) Right(Json.Null)
    else {
      try {
        val result = kvs.foldLeft[Json](Json.Null) { case (acc, (path, value)) =>
          if (path.nodes.isEmpty) value
          else {
            val built = buildFromKV(path, value)
            if (acc == Json.Null) built
            else acc.merge(built, overlayStrategy)
          }
        }
        Right(result)
      } catch {
        case e: Exception => Left(JsonError(s"Failed to assemble JSON from KV: ${e.getMessage}"))
      }
    }
  }

  private val overlayStrategy: MergeStrategy = MergeStrategy.Custom { (_, v1, v2) =>
    (v1, v2) match {
      case (Json.Object(flds1), Json.Object(flds2)) =>
        val map1 = flds1.toMap
        val map2 = flds2.toMap
        val allKeys = (map1.keys ++ map2.keys).toVector.distinct
        val merged = allKeys.map { k =>
          (map1.get(k), map2.get(k)) match {
            case (Some(c1), Some(c2)) => (k, c1.merge(c2, overlayStrategy))
            case (Some(c1), None)     => (k, c1)
            case (None, Some(c2))     => (k, c2)
            case (None, None)         => (k, Json.Null)
          }
        }
        Json.Object(merged)
      case (Json.Array(e1), Json.Array(e2)) =>
        val maxLen = math.max(e1.length, e2.length)
        val merged = (0 until maxLen).map { i =>
          val c1 = if (i < e1.length) e1(i) else Json.Null
          val c2 = if (i < e2.length) e2(i) else Json.Null
          (c1, c2) match {
            case (Json.Null, _) => c2
            case (_, Json.Null) => c1
            case _              => c1.merge(c2, overlayStrategy)
          }
        }.toVector
        Json.Array(merged)
      case _ => v2
    }
  }

  private def buildFromKV(path: DynamicOptic, value: Json): Json =
    path.nodes.foldRight(value) { (node, acc) =>
      node match {
        case DynamicOptic.Node.Field(name)   => Json.Object(Vector((name, acc)))
        case DynamicOptic.Node.AtIndex(idx)  =>
          val arr = Vector.fill(idx)(Json.Null) :+ acc
          Json.Array(arr)
        case _ => acc
      }
    }

  /**
   * Assembles JSON from path-value pairs, throwing on conflict.
   */
  def fromKVUnsafe(kvs: Seq[(DynamicOptic, Json)]): Json =
    fromKV(kvs).fold(throw _, identity)

  // ===========================================================================
  // Ordering
  // ===========================================================================

  /**
   * Ordering for JSON values.
   *
   * Order: Null < Boolean < Number < String < Array < Object
   */
  implicit val ordering: Ordering[Json] = (x: Json, y: Json) => x.compare(y)
}

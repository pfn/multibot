final class doc(help: String)
  extends annotation.Annotation
  with annotation.StaticAnnotation
object ffbe {
  private[this] val rainbow11 = 1 - (0.95 * math.pow(0.99, 10))

  private[this] def rainbow_11_d(n: Int): Double = n match {
    case 1 => rainbow11
    case x if x < 1 => 0.0
    case _ => 1 - (1 - rainbow_11_d(n - 1)) * (1 - rainbow11)
  }

  def help(function: String): String = {
    import reflect.runtime.universe._
    val t = typeOf[ffbe.type]
    val members = t.typeSymbol.typeSignature.members
    val matches = members.filter { s =>
      s.name.toString == function && s.isPublic && s.isMethod && !s.isSynthetic
    }
    if (matches.isEmpty) {
      val fs = members.collect {
        case s if s.isPublic && s.owner.asType == t.typeSymbol &&
          !s.isSynthetic && s.isMethod &&
            s.asMethod.paramLists.head.nonEmpty => s.name.toString
      }.toList.distinct.sorted.mkString(", ")
      s"Available functions:\n$fs"
    } else {
      val annot = typeOf[doc]
      val annos = matches.collect {
        case s => s.annotations.filter(_.tree.tpe == annot)
      }.flatMap(identity)
      val h = annos.map(_.tree.children.tail match {
        case List(Literal(Constant(s: String))) => s
        case x => showRaw(x)
      }).mkString
      (if (h.nonEmpty) h + "\n\n" else "") + matches.map { s =>
        "  " + s.fullName +
          org.multibot.DiscordMultibot.mapParams(
            runtimeMirror(this.getClass.getClassLoader),
            t.termSymbol.asModule, s.name.toString,
            s.asMethod.paramLists.head, withtype = true) +
              ": " + s.asMethod.returnType
      }.mkString("\n")
    }
  }

  @doc(
    "calculate the chance of obtaining at least 1 rainbow in `n` 10+1 pulls")
  def rainbow_11(n: Int): String =
    f"${math.min(0.9999, rainbow_11_d(n)) * 100}%.02f%%"

  @doc("calculate the healing per turn, given the specified inputs")
  def healing(ratio: Double, spr: Int, mag: Int,
    base: Int = 0, turns: Int = 1): Int =
      (ratio * (spr * 0.5 + mag * 0.1) + base).toInt / turns

  @doc("calculate the effective ratio of a skill given `ignore` def/spr")
  def ignore_def(ratio: Double, ignore: Double) = ratio / (1 - ignore)

  case class Variance(min: Int, avg: Int, max: Int) {
    override def toString =
      s"(min = $min, avg = $avg, max = $max)"
    def *(factor: Double) = Variance((min * factor).toInt,
      (avg * factor).toInt, (max * factor).toInt)
    def /(factor: Double) = Variance((min / factor).toInt,
      (avg / factor).toInt, (max / factor).toInt)
    def +(other: Variance) =
      Variance(min + other.min, avg + other.avg, max + other.max)
  }

  @doc("Calculate damage variance using the given `min`/`max` percentages")
  def variance(min: Double, max: Double, value: Int): Variance = {
    Variance((value * math.min(min, max)).toInt,
      ((math.abs(max - min)/2 + math.min(max,min)) * value).toInt,
      (value * math.max(min, max)).toInt)
  }

  def variance(min: Double, max: Double, value: Variance): Variance = {
    Variance((value.min * math.min(min, max)).toInt,
      ((math.abs(max - min)/2 + math.min(max,min)) * value.avg).toInt,
      (value.max * math.max(min, max)).toInt)
  }
  def variance(min: Double, max: Double, value: Hybrid): Variance = {
    Variance((value.physical * math.min(min, max)).toInt + value.magical,
      ((math.abs(max - min)/2 + math.min(max,min)) * value.physical).toInt +
        value.magical,
      (value.physical * math.max(min, max)).toInt + value.magical)
  }

  @doc("Calculate damage variance including the final 85-100% variance")
  def fvariance(min: Double, max: Double, value: Int): Variance = {
    variance(0.85, 1, variance(min, max, value))
  }

  def fvariance(min: Double, max: Double, value: Hybrid): Variance = {
    variance(0.85, 1, variance(min, max, value))
  }

  def fvariance(value: Int): Variance = variance(0.85, 1, value)

  @doc("[WRONG] Calculate the damage of an esper, stats=atk+mag+def+spr of esper")
  def esper(stats: Double, ratio: Double, defs: Int = 10) =
    ((math.pow(0.01 * stats.toInt, 2)/defs).toInt * ratio).toInt

  @doc("Calculate the lapis cost to farm 0-100% trust given max `nrg` and `seconds` runtime")
  def tmr(nrg: Int, seconds: Int = 36): String =
    s"${(10000 - ((10000 / (3600 / seconds)) * 12)) / nrg * 100} lapis"

  @doc("Generate a lazy list of damage ratios per given chain hit")
  def chain_stream(elements: Int = 1,
    spark: Boolean = false): Stream[Double] = {
    val bonus  = elements * 0.2 + 0.1

    def s(i: Int, m: Double): Stream[Double] = m #:: s(i + 1, {
      // in a normal chain, only every other hit can receive spark bonus
      val b = if (i % 2 == 0 && spark) bonus + 0.3 else bonus
      math.min(m + b, 4.0)
    })
    s(0, 1)
  }

  @doc("Calculate the average damage ratio across `hits` in a chain")
  def chain_ratio(
    hits: Int, elements: Int = 1, spark: Boolean = false): Double =
      chain_stream(elements, spark).take(hits).sum / hits

  @doc("Calculate damage given inputs, `its` = ignore spr")
  def magical(
    mag:       Int,
    ratio:     Double,
    killer:    Double = 0,
    elemental: Double = 0,
    spr:       Int    = 10,
    its:       Double = 0.0,
    level:     Int    = 100): Int = {
    ((math.pow(mag, 2) / (spr * (1 - its))).toInt *
      (1 + killer) * math.max(0, (1 + elemental)) *
        (1 + (level / 100)) * ratio).toInt
  }

  @doc("Calculate damage given inputs, `elemental2` = element debuff on skill, `itd` = ignore def, `defs` = def")
  def physical(
    atk:        Int,
    ratio:      Double,
    killer:     Double  = 0,
    elemental:  Double  = 0,
    elemental2: Double  = 0,
    defs:       Int     = 10,
    itd:        Double  = 0.0,
    dw:         Boolean = true,
    level:      Int     = 100,
    l:          Int     = 0,
    r:          Int     = 0): Int = {
    if (dw) {
      physical(atk - l,
        ratio, killer, elemental, 0, defs, itd, false, level, 0) +
          physical(atk - r,
            ratio, killer, elemental + elemental2, 0, defs, itd, false, level, 0)
    } else {
      ((math.pow(atk, 2) / (defs * (1 - itd))).toInt *
        (1 + killer) * math.max(0, (1 + elemental)) *
          (1 + (level / 100)) * ratio).toInt
    }
  }

  case class Hybrid(physical: Int, magical: Int, hybrid: Int) {
    override def toString =
      s"(physical = $physical, magical = $magical, hybrid = $hybrid)"
    def *(factor: Double) =
      Hybrid((physical * factor).toInt, (magical * factor).toInt,
        (hybrid * factor).toInt)
    def /(factor: Double) =
      Hybrid((physical / factor).toInt, (magical / factor).toInt,
        (hybrid / factor).toInt)
    def +(other: Hybrid) = Hybrid(physical + other.physical,
      magical + other.magical, hybrid + other.magical)
  }

  @doc("Calculate damage given inputs, `elemental2` = elemental debuff on skill, `itd` = ignore def, `its` = ignore spr, `defs` = def")
  def hybrid(
    atk:        Int,
    mag:        Int,
    ratio:      Double,
    killer:     Double  = 0,
    elemental:  Double  = 0,
    elemental2: Double  = 0,
    defs:       Int     = 10,
    spr:        Int     = 10,
    itd:        Double  = 0,
    its:        Double  = 0,
    dw:         Boolean = true,
    level:      Int     = 100,
    l:          Int     = 0,
    r:          Int     = 0): Hybrid = {
    if (dw) {
      val Hybrid(p1, m1, h1) = hybrid(atk - l,
        mag, ratio, killer, elemental, 0, defs, spr, itd, its, false, level)

      val Hybrid(p2, m2, h2) = hybrid(atk - r,
        mag, ratio, killer, elemental + elemental2,
          0, defs, spr, itd, its, false, level)
      Hybrid(p1+p2, m1+m2, h1+h2)
    } else {
      val p = physical(atk,
        ratio, killer, elemental, 0, defs, itd, false, level, l, r) / 2
      val m = magical(mag, ratio, killer, elemental, spr, its, level) / 2
      Hybrid(p, m, p+m)
    }
  }
}

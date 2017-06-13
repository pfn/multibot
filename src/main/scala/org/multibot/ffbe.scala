object ffbe {
  private[this] val rainbol1 = 1 - (0.95 * math.pow(0.99, 10))

  def rainbow_11_d(n: Int): Double = n match {
    case 0 => 0.0
    case 1 => rainbol1
    case _ => 1 - (1-rainbow_11_d(n - 1)) * (1-rainbol1)
  }

  def rainbow_11(n: Int): String =
    f"${math.min(0.9999, rainbow_11_d(n)) * 100}%.02f%%"

  def healing(ratio: Double, spr: Int, mag: Int): Int =
    (ratio * (spr * 0.5 + mag * 0.1)).toInt

  def ignore_def(ratio: Double, ignore: Double): String =
    f"${ratio/(1-ignore)}%.02f"

  def tmr(nrg: Int, phr: Int = 100): String =
    s"${(10000 - ((10000 / phr) * 12)) / nrg * 100} lapis"

  def chain_ratio(hits: Int,
    elemental: Boolean = true,
    spark:     Boolean = false,
    extra:     Boolean = false): String = {

    val bonus_ = if (elemental) 0.3 else 0.1
    val bonus  = if (extra) 0.2 + bonus_ else bonus_

    val (_,ratio,count) = Stream.from(0).zipWithIndex.map { case (m,i) =>
      // in a normal chain, only every other hit can receive spark bonus
      val b = if (i % 2 == 1 && spark) bonus + 0.2 else bonus
      (m * b + 1, i)
    }.takeWhile { case (m,h) =>
      m < 4 && h < hits
    }.foldLeft((0.0, 0.0, 0)) {
      case ((total,mult,hits),(m,i)) =>
        (total + m, (total + m) / (i + 1), i + 1)
    }

    val building_hits = math.min(count, hits)
    val max_hits = math.max(0, hits - count)
    val r = (building_hits * ratio + max_hits * 4) / hits
    f"$r%.02f"
  }

  def magical(
    mag:       Int,
    ratio:     Double,
    killer:    Double = 0,
    elemental: Double = 0,
    spr:       Int    = 10,
    its:       Double = 0.0,
    level:     Int    = 100): Int = {
    ((math.pow(mag, 2)/(spr*(1-its))).toInt *
      (1+killer) * math.max(0,(1+elemental)) * (1 + (level/100)) * ratio).toInt
  }

  def physical(
    atk:        Int,
    ratio:      Double,
    killer:     Double  = 0,
    elemental:  Double  = 0,
    relemental: Double = 0,
    defs:       Int     = 10,
    itd:        Double  = 0.0,
    dw:         Boolean = true,
    level:      Int     = 100,
    l:          Int     = 0,
    r:          Int     = 0): Int = {
    if (dw) {
      physical(atk - r, ratio, killer, elemental, 0, defs, itd, false, level, 0) +
        physical(atk - l, ratio, killer, elemental, relemental, defs, itd, false, level, 0)
    } else {
      ((math.pow(atk, 2)/(defs*(1-itd))).toInt *
        (1+killer) * math.max(0,(1+elemental+relemental)) * (1 + (level/100)) * ratio).toInt
    }
  }

  def hybrid(
    atk:        Int,
    mag:        Int,
    ratio:      Double,
    killer:     Double  = 0,
    elemental:  Double  = 0,
    relemental: Double  = 0,
    defs:       Int     = 10,
    spr:        Int     = 10,
    itd:        Double  = 0,
    its:        Double  = 0,
    dw:         Boolean = true,
    level:      Int     = 100,
    l:          Int     = 0,
    r:          Int     = 0): (Int,Int,Int) = {
    if (dw) {
      val (p1,m1,h1) = hybrid(atk - l, mag, ratio, killer, elemental, 0, defs, spr, itd, its, false, level)
      val (p2,m2,h2) = hybrid(atk - r, mag, ratio, killer, elemental + relemental, 0, defs, spr, itd, its, false, level)
      (p1+p2,m1+m2,h1+h2)
    } else {
      val p = physical(atk, ratio, killer, elemental, 0, defs, itd, false, level, l, r)/2
      val m = magical(mag, ratio, killer, elemental, spr, its, level) / 2
      (p,m,p+m)
    }
  }
}

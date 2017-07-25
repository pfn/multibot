import org.scalatest.FunSuite

class FfbeTest extends FunSuite {
  test("chain frames") {
    import ffbe._
    val dr = chain_frames("70,7,5,7,7,7,7")
    val ffb = chain_frames("40,7,5,7,7,7,7", cast=8)
    val qt = chain_frames("22,5,5,5,5,5,5,5,5,5,5,20", cast=20)
    val bs = chain_frames("40-25-25-25-25-25-25-25-25")
    solve_chain(dr, ffb)
    info("---")
    solve_chain(dr, dr)
    info("---")
    solve_chain(qt, qt)
    info("---")
    solve_chain(bs, bs)
  }
}

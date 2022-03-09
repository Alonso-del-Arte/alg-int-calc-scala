package calculators

import algebraics.quadratics.ImagQuadRing
import viewers.ImagQuadRingDisplay

object AlgIntCalc {

  def main(args: Array[String]): Unit = {
    if (System.getProperty("os.name") == "Mac OS X") System.setProperty("apple.laf.useScreenMenuBar", "true")
    println("Still working on the main program...")
    println("In the meantime, please enjoy the imaginary quadratic ring viewer...")
    val initRing = new ImagQuadRing(-3)
    val iqrd = new ImagQuadRingDisplay(initRing)
    iqrd.startRingDisplay()
  }

}

package viewers

import algebraics.quadratics.{ImagQuadRing, QuadRing}
import calculators.NumberTheoreticFunctionsCalculator

import java.awt.Graphics
import java.awt.event.MouseEvent
import java.net.{URI, URISyntaxException}

object ImagQuadRingDisplay {

  val MINIMUM_RING_D: Int = -8191

}

class ImagQuadRingDisplay(ring: ImagQuadRing) extends RingDisplay(ring) {
  this.pixelsPerBasicImagInterval = Math.floor(this.pixelsPerUnitInterval * this.ring.getAbsNegRadSqrt).toInt

  private def drawGrids(g: Graphics): Unit = {
    val maxImLineNumber = Math.floor(this.zeroCoordY / this.pixelsPerBasicImagInterval).toInt
    val maxReLineNumber = Math.floor(this.zeroCoordX / this.pixelsPerUnitInterval).toInt
    g.setColor(this.halfIntegerGridColor)
    if (this.diagramRing.asInstanceOf[QuadRing].hasHalfIntegers) {
      val halfImagInterval = Math.floor(this.pixelsPerBasicImagInterval / 2).toInt
      for (currImLine <- 0 to maxImLineNumber) {
        val currNeg = this.zeroCoordY - halfImagInterval - currImLine * this.pixelsPerBasicImagInterval
        g.drawLine(0, currNeg, this.ringCanvasHorizMax, currNeg)
        val currPos = this.zeroCoordY + halfImagInterval + currImLine * this.pixelsPerBasicImagInterval
        g.drawLine(0, currPos, this.ringCanvasHorizMax, currPos)
      }
      val halfUnitInterval = Math.floor(this.pixelsPerUnitInterval / 2).toInt
      for (currReLine <- 0 to maxReLineNumber) {
        val currNeg = this.zeroCoordX - halfUnitInterval - currReLine * this.pixelsPerUnitInterval
        g.drawLine(currNeg, 0, currNeg, this.ringCanvasVerticMax)
        val currPos = this.zeroCoordX + halfUnitInterval + currReLine * this.pixelsPerUnitInterval
        g.drawLine(currPos, 0, currPos, this.ringCanvasVerticMax)
      }
    }
    g.setColor(this.integerGridColor)
    g.drawLine(0, this.zeroCoordY, this.ringCanvasHorizMax, this.zeroCoordY)
    for (currImLine <- 1 to maxImLineNumber) {
      val currNeg = this.zeroCoordY + currImLine * this.pixelsPerBasicImagInterval
      g.drawLine(0, currNeg, this.ringCanvasHorizMax, currNeg)
      val currPos = this.zeroCoordY - currImLine * this.pixelsPerBasicImagInterval
      g.drawLine(0, currPos, this.ringCanvasHorizMax, currPos)
    }
    g.drawLine(this.zeroCoordX, 0, this.zeroCoordX, this.ringCanvasVerticMax)
    for (currReLine <- 1 to maxReLineNumber) {
      val currNeg = this.zeroCoordX - currReLine * this.pixelsPerUnitInterval
      g.drawLine(currNeg, 0, currNeg, this.ringCanvasVerticMax)
      val currPos = this.zeroCoordX + currReLine * this.pixelsPerUnitInterval
      g.drawLine(currPos, 0, currPos, this.ringCanvasVerticMax)
    }
  }

  private def drawPoints(g: Graphics): Unit = {
    g.setColor(this.zeroColor)
    g.fillOval(this.zeroCoordX - this.dotRadius, this.zeroCoordY - this.dotRadius, this.dotDiameter, this.dotDiameter)
    g.setColor(this.unitColor)
    g.fillOval(this.zeroCoordX + this.pixelsPerUnitInterval - this.dotRadius, this.zeroCoordY - this.dotRadius, this.dotDiameter, this.dotDiameter)
    g.fillOval(this.zeroCoordX - this.pixelsPerUnitInterval - this.dotRadius, this.zeroCoordY - this.dotRadius, this.dotDiameter, this.dotDiameter)
    if (this.diagramRing.asInstanceOf[ImagQuadRing].radicand == -1) {
      g.fillOval(this.zeroCoordX - this.dotRadius, this.zeroCoordY + this.pixelsPerUnitInterval - this.dotRadius, this.dotDiameter, this.dotDiameter)
      g.fillOval(this.zeroCoordX - this.dotRadius, this.zeroCoordY - this.pixelsPerUnitInterval - this.dotRadius, this.dotDiameter, this.dotDiameter)
    }
    val halfUnitInterval = Math.floor(this.pixelsPerUnitInterval / 2).toInt
    val halfImagInterval = Math.floor(this.pixelsPerBasicImagInterval / 2).toInt
    if (this.diagramRing.asInstanceOf[ImagQuadRing].radicand == -3) {
      g.fillOval(this.zeroCoordX - halfUnitInterval - this.dotRadius, this.zeroCoordY + halfImagInterval - this.dotRadius, this.dotDiameter, this.dotDiameter)
      g.fillOval(this.zeroCoordX + halfUnitInterval - this.dotRadius, this.zeroCoordY + halfImagInterval - this.dotRadius, this.dotDiameter, this.dotDiameter)
      g.fillOval(this.zeroCoordX + halfUnitInterval - this.dotRadius, this.zeroCoordY - halfImagInterval - this.dotRadius, this.dotDiameter, this.dotDiameter)
      g.fillOval(this.zeroCoordX - halfUnitInterval - this.dotRadius, this.zeroCoordY - halfImagInterval - this.dotRadius, this.dotDiameter, this.dotDiameter)
    }
    val maxReLineNumber = Math.floor(this.zeroCoordX / this.pixelsPerUnitInterval).toInt
    val maxImLineNumber = Math.floor(this.zeroCoordY / this.pixelsPerBasicImagInterval).toInt
  }

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    if (this.pixelsPerUnitInterval > RingDisplay.MINIMUM_PIXELS_PER_UNIT_INTERVAL_TO_DRAW_GRIDS) this.drawGrids(g)
    this.drawPoints(g)
  }

  override protected def setPixelsPerBasicImaginaryInterval(): Unit = {
    this.pixelsPerBasicImagInterval = Math.floor(this.pixelsPerUnitInterval *
      this.diagramRing.asInstanceOf[QuadRing].getAbsNegRadSqrt).toInt
  }

  override def chooseDiscriminant(): Unit = {}

  override def incrementDiscriminant(): Unit = {
    var discr = this.diagramRing.asInstanceOf[QuadRing].radicand + 1
    while (!NumberTheoreticFunctionsCalculator.isSquarefree(discr) && discr < -1) discr += 1
    if (discr == -1) this.increaseDMenuItem.setEnabled(false)
    if (discr == ImagQuadRingDisplay.MINIMUM_RING_D + 1) this.decreaseDMenuItem.setEnabled(true)
    val ring = new ImagQuadRing(discr)
    this.switchToRing(ring)
  }

  override def decrementDiscriminant(): Unit = {
    var discr = this.diagramRing.asInstanceOf[QuadRing].radicand - 1
    while (!NumberTheoreticFunctionsCalculator.isSquarefree(discr) && discr > ImagQuadRingDisplay.MINIMUM_RING_D) discr -= 1
    if (discr == ImagQuadRingDisplay.MINIMUM_RING_D) this.decreaseDMenuItem.setEnabled(false)
    if (discr == -2) this.increaseDMenuItem.setEnabled(true)
    val ring = new ImagQuadRing(discr)
    this.switchToRing(ring)
  }

  override def mouseDragged(e: MouseEvent): Unit = {}

  override def mouseMoved(e: MouseEvent): Unit = {}

  override def getAboutBoxMessage: String = "Imaginary Quadratic Ring Display\nVersion 0.0\n\\u00A9 2019 Alonso del Arte"

  override def getUserManualURL: URI = {
    val urlStr = "https://github.com/Alonso-del-Arte/visualization-quadratic-imaginary-rings/blob/master/dist-jar/README.md"
    try {
      val url = new URI(urlStr)
      url
    } catch {
      case urise: URISyntaxException => throw new RuntimeException(urise)
    }
  }

}

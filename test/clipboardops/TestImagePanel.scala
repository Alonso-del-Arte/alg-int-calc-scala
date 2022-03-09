package clipboardops

import java.awt.{Color, Dimension, Font, Graphics}

import javax.swing.{JFrame, JPanel, WindowConstants}

object TestImagePanel {

  val PANEL_HEIGHT = 300
  val PANEL_WIDTH = 400

  val PREFERRED_DIMENSION = new Dimension(PANEL_WIDTH, PANEL_HEIGHT)

  val BACKGROUND_COLOR = new Color(255)
  val MEDIUM_GREEN = new Color(32896)

}

class TestImagePanel extends JPanel {
  this.setBackground(TestImagePanel.BACKGROUND_COLOR)
  this.setPreferredSize(TestImagePanel.PREFERRED_DIMENSION)
  private val frame = new JFrame("Test Image")
  this.showPanel()

  override def paintComponent(g: Graphics): Unit = {
    val oneThirdHeight = TestImagePanel.PANEL_HEIGHT / 3
    g.setColor(TestImagePanel.BACKGROUND_COLOR)
    g.fillRect(0, 0, TestImagePanel.PANEL_WIDTH, oneThirdHeight)
    g.setColor(TestImagePanel.MEDIUM_GREEN)
    val twoThirdsHeight = 2 * oneThirdHeight
    g.fillRect(0, oneThirdHeight, TestImagePanel.PANEL_WIDTH, twoThirdsHeight)
    g.setColor(Color.BLACK)
    val font = new Font(g.getFont.getFontName, Font.PLAIN, 192)
    g.setFont(font)
    g.drawString("\u68EE", 50, TestImagePanel.PANEL_HEIGHT - 50)
  }

  def showPanel(): Unit = {
    frame.add(this)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.pack()
    frame.setVisible(true)
  }

  def closePanel(): Unit = {
    frame.dispose()
  }

}

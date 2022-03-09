package viewers

import algebraics.{AlgInt, IntRing}
import algebraics.unary.UnaryInt
import clipboardops.ImageSelection
import fileops.{FileChooserWithOverwriteGuard, PNGFileFilter}

import java.awt.{BorderLayout, Color, Desktop, Dimension, Event, Graphics}
import java.awt.datatransfer.StringSelection
import java.awt.event.{ActionEvent, ActionListener, KeyEvent, MouseMotionListener}
import java.awt.image.BufferedImage
import java.io.{File, IOException}
import java.net.URI

import javax.imageio.ImageIO
import javax.swing.{JCheckBoxMenuItem, JFileChooser, JFrame, JLabel, JMenu, JMenuBar, JMenuItem, JOptionPane, JPanel, JTextField, KeyStroke}

import scala.collection.mutable.ArrayBuffer

object RingDisplay {
  private var windowCount: Int = 0
  private var haveSavedBefore = false
  private var prevSavePathName = ""
  private val macOSFlag = System.getProperty("os.name").equals("Mac OS X")
  private val maskCtrlCommand = if (macOSFlag) Event.META_MASK else Event.CTRL_MASK
  val DEFAULT_PIXELS_PER_UNIT_INTERVAL = 40
  val MINIMUM_PIXELS_PER_UNIT_INTERVAL = 2
  val MINIMUM_PIXELS_PER_UNIT_INTERVAL_TO_DRAW_GRIDS = 5
  val MAXIMUM_PIXELS_PER_UNIT_INTERVAL = 6400
  val RING_CANVAS_HORIZ_MIN = 100
  val RING_CANVAS_VERTIC_MIN = 178
  val RING_CANVAS_DEFAULT_HORIZ_MAX = 1280
  val RING_CANVAS_DEFAULT_VERTIC_MAX = 720
  val PURELY_REAL_RING_CANVAS_DEFAULT_VERTIC_MAX = 180
  val DEFAULT_DOT_RADIUS = 5
  val MINIMUM_DOT_RADIUS = 1
  val MAXIMUM_DOT_RADIUS = 128
  val DEFAULT_ZOOM_STEP = 5
  val MINIMUM_ZOOM_STEP = 1
  val MAXIMUM_ZOOM_STEP = 48
  val DEFAULT_CANVAS_BACKGROUND_COLOR = new Color(2107440)
  val DEFAULT_HALF_INTEGER_GRID_COLOR: Color = Color.DARK_GRAY
  val DEFAULT_INTEGER_GRID_COLOR: Color = Color.BLACK
  val DEFAULT_ZERO_COLOR: Color = Color.BLACK
  val DEFAULT_UNIT_COLOR: Color = Color.WHITE
  val DEFAULT_INERT_PRIME_COLOR: Color = Color.CYAN
  val DEFAULT_SPLIT_PRIME_COLOR: Color = Color.BLUE
  val DEFAULT_SPLIT_MID_DEGREE_PRIME_COLOR = new Color(24831)
  val DEFAULT_RAMIFIED_PRIME_COLOR: Color = Color.GREEN
  val DEFAULT_RAMIFIED_MID_DEGREE_PRIME_COLOR = new Color(65376)
  val DEFAULT_READOUT_FIELD_COLUMNS = 20
  val MAXIMUM_HISTORY_ITEMS = 128

  def makeMenuItem(menuText: String, accessibleDescription: String, actionCmd: String, keyStroke: KeyStroke, listener: ActionListener): JMenuItem = {
    val menuItem = new JMenuItem(menuText)
    menuItem.getAccessibleContext.setAccessibleDescription(accessibleDescription)
    menuItem.setActionCommand(actionCmd)
    menuItem.setAccelerator(keyStroke)
    menuItem.addActionListener(listener)
    menuItem
  }

}

abstract class RingDisplay(ring: IntRing) extends JPanel with ActionListener with MouseMotionListener {
  protected var alreadySetUp = false
  protected var includeRingChoice = true
  protected var includeReadoutsUpdate = true
  protected var includeThetaToggle = true
  protected var diagramRing: IntRing = ring
  protected var mouseAlgInt: AlgInt = new UnaryInt(0)
  protected var pixelsPerUnitInterval: Int = RingDisplay.DEFAULT_PIXELS_PER_UNIT_INTERVAL
  protected var pixelsPerBasicImagInterval: Int = this.pixelsPerUnitInterval
  protected var preferenceForThetaNotation = false
  protected var ringCanvasHorizMax: Int = RingDisplay.RING_CANVAS_DEFAULT_HORIZ_MAX
  protected var ringCanvasVerticMax: Int = RingDisplay.RING_CANVAS_DEFAULT_VERTIC_MAX
  protected var dotRadius: Int = RingDisplay.DEFAULT_DOT_RADIUS
  protected var dotDiameter: Int = 2 * this.dotRadius
  protected var zoomStep: Int = RingDisplay.DEFAULT_ZOOM_STEP
  protected var backgroundColor: Color = RingDisplay.DEFAULT_CANVAS_BACKGROUND_COLOR
  protected var halfIntegerGridColor: Color = RingDisplay.DEFAULT_HALF_INTEGER_GRID_COLOR
  protected var integerGridColor: Color = RingDisplay.DEFAULT_INTEGER_GRID_COLOR
  protected var zeroColor: Color = RingDisplay.DEFAULT_ZERO_COLOR
  protected var unitColor: Color = RingDisplay.DEFAULT_UNIT_COLOR
  protected var inertPrimeColor: Color = RingDisplay.DEFAULT_INERT_PRIME_COLOR
  protected var splitPrimeColor: Color = RingDisplay.DEFAULT_SPLIT_PRIME_COLOR
  protected var splitPrimeMidDegreeColor: Color = RingDisplay.DEFAULT_SPLIT_MID_DEGREE_PRIME_COLOR
  protected var ramifiedPrimeColor: Color = RingDisplay.DEFAULT_RAMIFIED_PRIME_COLOR
  protected var ramifiedPrimeMidDegreeColor: Color = RingDisplay.DEFAULT_RAMIFIED_MID_DEGREE_PRIME_COLOR
  protected var zeroCoordX: Int = Math.floor(this.ringCanvasHorizMax / 2).toInt
  protected var zeroCoordY: Int = Math.floor(this.ringCanvasVerticMax / 2).toInt
  private[this] val initialTitle = "Ring diagram for " + this.diagramRing.toString
  protected var ringFrame = new JFrame(initialTitle)
  protected val discrHistory = new ArrayBuffer[IntRing]()
  discrHistory += this.diagramRing
  protected var currHistoryIndex = 0
  protected val increaseDMenuItem: JMenuItem = RingDisplay.makeMenuItem("Increment parameter d",
    "Increment parameter d to choose another ring",
    "incrD", if (RingDisplay.macOSFlag) {
      KeyStroke.getKeyStroke(KeyEvent.VK_Y, RingDisplay.maskCtrlCommand)
    } else {
      KeyStroke.getKeyStroke(KeyEvent.VK_UP, RingDisplay.maskCtrlCommand)
    }, this)
  protected val decreaseDMenuItem: JMenuItem = RingDisplay.makeMenuItem("Decrement parameter d",
    "Decrement parameter d to choose another ring",
    "decrD", if (RingDisplay.macOSFlag) {
      KeyStroke.getKeyStroke(KeyEvent.VK_B, RingDisplay.maskCtrlCommand)
    } else {
      KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, RingDisplay.maskCtrlCommand)
    }, this)
  protected val prevDMenuItem: JMenuItem = RingDisplay.makeMenuItem("Previous parameter d",
    "View the diagram for the previous discriminant", "prevD",
    KeyStroke.getKeyStroke(KeyEvent.VK_G, RingDisplay.maskCtrlCommand), this)
  protected val nextDMenuItem: JMenuItem = RingDisplay.makeMenuItem("Next parameter d",
    "View the diagram for the next discriminant", "nextD",
    KeyStroke.getKeyStroke(KeyEvent.VK_J, RingDisplay.maskCtrlCommand), this)
  protected val zoomInMenuItem: JMenuItem = RingDisplay.makeMenuItem("Zoom in",
    "Zoom in, by increasing pixels per unit interval", "zoomIn",
    if (RingDisplay.macOSFlag) {
      KeyStroke.getKeyStroke(KeyEvent.VK_EQUALS, Event.SHIFT_MASK)
    } else {
      KeyStroke.getKeyStroke(KeyEvent.VK_ADD, Event.CTRL_MASK)
    }, this)
  protected val zoomOutMenuItem: JMenuItem = RingDisplay.makeMenuItem("Zoom out",
    "Zoom out, by decreasing pixels per unit interval", "zoomOut",
    if (RingDisplay.macOSFlag) {
      KeyStroke.getKeyStroke(KeyEvent.VK_MINUS, 0)
    } else {
      KeyStroke.getKeyStroke(KeyEvent.VK_SUBTRACT, Event.CTRL_MASK)
    }, this)
  protected val decreaseZoomStepMenuItem: JMenuItem = RingDisplay.makeMenuItem("Decrease zoom step",
    "Decrease the zoom step used by the zoom in and zoom out functions", "decrZoomStep",
    KeyStroke.getKeyStroke(KeyEvent.VK_COMMA, RingDisplay.maskCtrlCommand + Event.SHIFT_MASK), this)
  protected val increaseZoomStepMenuItem: JMenuItem = RingDisplay.makeMenuItem("Increase zoom step",
    "Increase the zoom step used by the zoom in and zoom out functions", "incrZoomStep",
    KeyStroke.getKeyStroke(KeyEvent.VK_PERIOD, RingDisplay.maskCtrlCommand + Event.SHIFT_MASK), this)
  private[this] val dotRadiusOrLineThicknessText = if (this.diagramRing.isPurelyReal) {
    "line thickness"
  } else {
    "dot radius"
  }
  private[this] val dotsOrLinesText = if (this.diagramRing.isPurelyReal) {
    "lines"
  } else {
    "dots"
  }
  protected val decreaseDotRadiusMenuItem: JMenuItem = RingDisplay.makeMenuItem("Decrease " + dotRadiusOrLineThicknessText,
    "Decrease the " + dotRadiusOrLineThicknessText + "used to draw the " + dotsOrLinesText, "decrDotRadius",
    KeyStroke.getKeyStroke(KeyEvent.VK_COMMA, RingDisplay.maskCtrlCommand), this)
  protected val increaseDotRadiusMenuItem: JMenuItem = RingDisplay.makeMenuItem("Increase " + dotRadiusOrLineThicknessText,
    "Increase the " + dotRadiusOrLineThicknessText + "used to draw the " + dotsOrLinesText, "incrDotRadius",
    KeyStroke.getKeyStroke(KeyEvent.VK_PERIOD, RingDisplay.maskCtrlCommand), this)
  protected val preferThetaNotationMenuItem: JCheckBoxMenuItem = new JCheckBoxMenuItem("Use theta notation in readouts", false)
  protected val toggleReadOutsEnabledMenuItem = new JCheckBoxMenuItem("Update readouts", false)
  protected val algIntReadOut: JTextField = new JTextField(RingDisplay.DEFAULT_READOUT_FIELD_COLUMNS)
  protected val algIntTraceReadOut: JTextField = new JTextField(RingDisplay.DEFAULT_READOUT_FIELD_COLUMNS)
  protected val algIntNormReadOut: JTextField = new JTextField(RingDisplay.DEFAULT_READOUT_FIELD_COLUMNS)
  protected val algIntPolReadOut: JTextField = new JTextField(RingDisplay.DEFAULT_READOUT_FIELD_COLUMNS)

  protected def setPixelsPerBasicImaginaryInterval(): Unit

  def setPixelsPerUnitInterval(pixelLength: Int): Unit = {
//    if (pixelLength < RingDisplay.MINIMUM_PIXELS_PER_UNIT_INTERVAL) {
//      val excMsg = "Pixels per unit interval setting needs to be greater than " + (RingDisplay.MINIMUM_PIXELS_PER_UNIT_INTERVAL - 1).toString
//      throw new IllegalArgumentException(excMsg)
//    }
//    if (pixelLength > RingDisplay.MAXIMUM_PIXELS_PER_UNIT_INTERVAL) {
//      val excMsg = "Pixels per unit interval setting needs to be less than " + (RingDisplay.MAXIMUM_PIXELS_PER_UNIT_INTERVAL + 1).toString
//      throw new IllegalArgumentException(excMsg)
//    }
    this.pixelsPerUnitInterval = pixelLength
    this.setPixelsPerBasicImaginaryInterval()
  }

  override def paintComponent(g: Graphics): Unit = super.paintComponent(g)

  // changeRingWindowDimensions PLACEHOLDER

//  def changeBackgroundColor(bgColor: Color): Unit = this.backgroundColor = bgColor
//
//  def changeGridColors(halfIntColor: Color, fullIntColor: Color): Unit = {
//    this.halfIntegerGridColor = halfIntColor
//    this.integerGridColor = fullIntColor
//  }
//
//  def changePointColors(zeroColor: Color, unitColor: Color, inertPrimeColor: Color, splitPrimeColor: Color, ramifiedPrimeColor: Color): Unit = {
//    this.zeroColor = zeroColor
//  }

  private def changeDotDiameter(): Unit = {
    this.dotDiameter = 2 * this.dotRadius
  }

  def changeDotRadius(radius: Int): Unit = {
//    if (radius < RingDisplay.MINIMUM_DOT_RADIUS) {
//      val excMsg = "New dot radius needs to be greater than " + (RingDisplay.MINIMUM_DOT_RADIUS + 1).toString
//      throw new IllegalArgumentException(excMsg)
//    }
//    if (radius > RingDisplay.MAXIMUM_DOT_RADIUS) {
//      val excMsg = "New dot radius needs to be less than " + (RingDisplay.MAXIMUM_DOT_RADIUS - 1).toString
//      throw new IllegalArgumentException(excMsg)
//    }
    this.dotRadius = radius
    this.changeDotDiameter()
  }

  def changeZoomStep(zoomStepPixels: Int): Unit = {
//    if (zoomStepPixels < RingDisplay.MINIMUM_ZOOM_STEP) {
//      val excMsg = "Zoom step needs to be at least " + RingDisplay.MINIMUM_ZOOM_STEP.toString + " pixels"
//      throw new IllegalArgumentException(excMsg)
//    }
//    if (zoomStepPixels > RingDisplay.MAXIMUM_ZOOM_STEP) {
//      val excMsg = "Zoom step should not be more than " + RingDisplay.MAXIMUM_ZOOM_STEP.toString + " pixels"
//      throw new IllegalArgumentException(excMsg)
//    }
    this.zoomStep = zoomStepPixels
  }

  // PLACEHOLDER changeZeroCoords()

  def saveDiagramAs(): Unit = {
    val diagram = new BufferedImage(this.ringCanvasHorizMax, this.ringCanvasVerticMax, BufferedImage.TYPE_INT_RGB)
    val graph = diagram.createGraphics()
    this.paint(graph)
    val suggestedFilename = this.diagramRing.toFilenameString + "pxui" + this.pixelsPerUnitInterval + ".png"
    var diagramFile = new File(suggestedFilename)
    val fileChooser = new FileChooserWithOverwriteGuard
    val pngFilter = new PNGFileFilter
    fileChooser.addChoosableFileFilter(pngFilter)
    if (RingDisplay.haveSavedBefore) {
      fileChooser.setCurrentDirectory(new File(RingDisplay.prevSavePathName))
    }
    fileChooser.setSelectedFile(diagramFile)
    val fcRet = fileChooser.showSaveDialog(this)
    fcRet match {
      case JFileChooser.APPROVE_OPTION => diagramFile = fileChooser.getSelectedFile
        val filePath = diagramFile.getAbsolutePath
        RingDisplay.prevSavePathName = filePath.substring(0, filePath.lastIndexOf(File.separator))
        RingDisplay.haveSavedBefore = true
        try {
          ImageIO.write(diagram, "PNG", diagramFile)
        } catch {
          case ioe: IOException => val notificationString = "Image input/output exception occurred:\n " + ioe.getMessage
            JOptionPane.showMessageDialog(this.ringFrame, notificationString)
        }
      case JFileChooser.CANCEL_OPTION => val notificationString = "File save canceled."
        JOptionPane.showMessageDialog(this.ringFrame, notificationString)
      case JFileChooser.ERROR_OPTION => val notificationString = "An error occurred trying to choose a file to save to."
        JOptionPane.showMessageDialog(this.ringFrame, notificationString)
      case _ => val notificationString = "Unexpected option " + fcRet + " from file chooser."
        JOptionPane.showMessageDialog(this.ringFrame, notificationString)
    }
  }

  protected def switchToRing(ring: IntRing): Unit = {
    this.ringFrame.setTitle("Ring diagram for " + ring.toString)
    this.setRing(ring)
    this.updateRingHistory(ring)
    this.repaint()
  }

  def chooseDiscriminant(): Unit

  def incrementDiscriminant(): Unit

  def decrementDiscriminant(): Unit

  def previousDiscriminant(): Unit = {
    this.currHistoryIndex -= 1
    this.switchToRing(this.discrHistory(this.currHistoryIndex))
    if (this.currHistoryIndex == 0) {
      this.prevDMenuItem.setEnabled(false)
    }
    if (!this.nextDMenuItem.isEnabled) {
      this.nextDMenuItem.setEnabled(true)
    }
  }

  def nextDiscriminant(): Unit = {
    this.currHistoryIndex += 1
    this.switchToRing(this.discrHistory(this.currHistoryIndex))
    if (this.currHistoryIndex == (this.discrHistory.size - 1)) {
      this.nextDMenuItem.setEnabled(false)
    }
    if (!this.prevDMenuItem.isEnabled) {
      this.prevDMenuItem.setEnabled(true)
    }
  }

  protected def updateRingHistory(ring: IntRing): Unit = {
    if (this.currHistoryIndex == (this.discrHistory.size - 1)) {
      this.discrHistory += ring
      this.currHistoryIndex += 1
      if (!this.prevDMenuItem.isEnabled) {
        this.prevDMenuItem.setEnabled(true)
      }
    } else {
      this.discrHistory(this.currHistoryIndex) = ring
      this.discrHistory.trimEnd(this.discrHistory.size - this.currHistoryIndex + 1)
      this.nextDMenuItem.setEnabled(false)
    }
    if (this.currHistoryIndex > RingDisplay.MAXIMUM_HISTORY_ITEMS) {
      this.discrHistory.trimStart(1) // Forget the first item
    }
  }

  def copyReadoutsToClipboard(): Unit = {
    val agregReadOuts = this.mouseAlgInt.toString +
      ", Trace: " + this.mouseAlgInt.trace.toString +
      ", Norm: " + this.mouseAlgInt.norm.toString +
      ", Polynomial: " + this.mouseAlgInt.minPolynomialString
    val stringSel = new StringSelection(agregReadOuts)
    this.getToolkit.getSystemClipboard.setContents(stringSel, stringSel)
  }

  def copyDiagramToClipboard(): Unit = {
    val diagram = new BufferedImage(this.ringCanvasHorizMax, this.ringCanvasVerticMax, BufferedImage.TYPE_INT_RGB)
    val graph = diagram.createGraphics()
    this.paint(graph)
    val imgSel = new ImageSelection(diagram)
    this.getToolkit.getSystemClipboard.setContents(imgSel, imgSel)
  }

  protected def checkZoomInOutEnablements(): Unit = {
    if (this.zoomInMenuItem.isEnabled) {
      if (this.pixelsPerUnitInterval > (RingDisplay.MAXIMUM_PIXELS_PER_UNIT_INTERVAL - this.zoomStep)) {
        this.zoomInMenuItem.setEnabled(false)
      }
    } else {
      if (this.pixelsPerUnitInterval <= (RingDisplay.MAXIMUM_PIXELS_PER_UNIT_INTERVAL - zoomStep)) {
        this.zoomInMenuItem.setEnabled(true)
      }
    }
    if (this.zoomOutMenuItem.isEnabled) {
      if (this.pixelsPerUnitInterval < (RingDisplay.MINIMUM_PIXELS_PER_UNIT_INTERVAL + zoomStep)) {
        this.zoomOutMenuItem.setEnabled(false)
      }
    } else {
      if (this.pixelsPerUnitInterval >= (RingDisplay.MINIMUM_PIXELS_PER_UNIT_INTERVAL + zoomStep)) {
        this.zoomOutMenuItem.setEnabled(true)
      }
    }
  }

  def zoomIn(): Unit = {
    val pixels = this.pixelsPerUnitInterval + this.zoomStep
    this.setPixelsPerUnitInterval(pixels)
    this.repaint()
    this.checkZoomInOutEnablements()
  }

  def zoomOut(): Unit = {
    val pixels = this.pixelsPerUnitInterval - this.zoomStep
    this.setPixelsPerUnitInterval(pixels)
    this.repaint()
    this.checkZoomInOutEnablements()
  }

  protected def checkZoomStepEnablements(): Unit = {
    if (this.decreaseZoomStepMenuItem.isEnabled) {
      if (this.zoomStep == RingDisplay.MINIMUM_ZOOM_STEP) {
        this.decreaseZoomStepMenuItem.setEnabled(false)
      }
    } else {
      if (this.zoomStep > RingDisplay.MINIMUM_ZOOM_STEP) {
        this.decreaseZoomStepMenuItem.setEnabled(true)
      }
    }
    if (this.increaseZoomStepMenuItem.isEnabled) {
      if (this.zoomStep == RingDisplay.MAXIMUM_ZOOM_STEP) {
        this.increaseZoomStepMenuItem.setEnabled(false)
      }
    } else {
      if (this.zoomStep < RingDisplay.MAXIMUM_ZOOM_STEP) {
        this.increaseZoomStepMenuItem.setEnabled(true)
      }
    }
  }

  protected def informZoomStepChange(): Unit = {
    val notificationString = "Zoom step is now " + this.zoomStep + ".\nThere are " + this.pixelsPerUnitInterval + " pixels per unit interval."
    JOptionPane.showMessageDialog(this.ringFrame, notificationString)
  }

  def decreaseZoomStep(): Unit = {
    this.changeZoomStep(this.zoomStep - 1)
    this.checkZoomInOutEnablements()
    this.checkZoomStepEnablements()
    this.informZoomStepChange()
  }

  def increaseZoomStep(): Unit = {
    this.changeZoomStep(this.zoomStep + 1)
    this.checkZoomInOutEnablements()
    this.checkZoomStepEnablements()
    this.informZoomStepChange()
  }

  protected def checkDotRadiusEnablements(): Unit = {
    if (this.decreaseDotRadiusMenuItem.isEnabled) {
      if (this.dotRadius == RingDisplay.MINIMUM_DOT_RADIUS) {
        this.decreaseDotRadiusMenuItem.setEnabled(false)
      }
    } else {
      if (this.dotRadius > RingDisplay.MINIMUM_DOT_RADIUS) {
        this.decreaseDotRadiusMenuItem.setEnabled(true)
      }
    }
    if (this.increaseDotRadiusMenuItem.isEnabled) {
      if (this.dotRadius == RingDisplay.MAXIMUM_DOT_RADIUS) {
        this.increaseDotRadiusMenuItem.setEnabled(false)
      }
    } else {
      if (this.dotRadius < RingDisplay.MAXIMUM_DOT_RADIUS) {
        this.increaseDotRadiusMenuItem.setEnabled(true)
      }
    }
  }

  def decreaseDotRadius(): Unit = {
    this.changeDotRadius(this.dotRadius - 1)
    this.repaint()
    this.checkDotRadiusEnablements()
  }

  def increaseDotRadius(): Unit = {
    this.changeDotRadius(this.dotRadius + 1)
    this.repaint()
    this.checkDotRadiusEnablements()
  }

  def resetViewDefaults(): Unit = {
    this.setPixelsPerUnitInterval(RingDisplay.DEFAULT_PIXELS_PER_UNIT_INTERVAL)
    this.changeZoomStep(RingDisplay.DEFAULT_ZOOM_STEP)
    this.changeDotRadius(RingDisplay.DEFAULT_DOT_RADIUS)
    this.checkZoomInOutEnablements()
    this.checkZoomStepEnablements()
    this.checkDotRadiusEnablements()
  }

  def toggleThetaNotation(): Unit = this.preferenceForThetaNotation = this.preferThetaNotationMenuItem.isSelected

  def toggleReadOutsEnabled(): Unit = {
    if (this.toggleReadOutsEnabledMenuItem.isEnabled) {
      this.addMouseMotionListener(this)
    } else {
      this.removeMouseMotionListener(this)
    }
  }

  def getUserManualURL: URI

  def showUserManual(): Unit = {
    val url = this.getUserManualURL
    if (Desktop.isDesktopSupported) {
      try {
        val desktop = Desktop.getDesktop
        desktop.browse(url)
      } catch {
        case ioe: IOException => val msg = "Sorry, unable to open URL\n<" + url.toString + ">\n\"" + ioe.getMessage + "\""
          JOptionPane.showMessageDialog(this.ringFrame, msg)
          System.err.println(msg)
      }
    } else {
      val msg = "Sorry, unable to open URL\n<" + url.toString + ">\nDefault Web browser is not available from this program."
      JOptionPane.showMessageDialog(this.ringFrame, msg)
      System.err.println(msg)
    }
  }

  def getAboutBoxMessage: String

  def showAboutBoxMessage(): Unit = JOptionPane.showMessageDialog(this.ringFrame, this.getAboutBoxMessage, "About", JOptionPane.PLAIN_MESSAGE)

  override def actionPerformed(ae: ActionEvent): Unit = {
    val command = ae.getActionCommand
    command match {
      case "saveDiagramAs" => this.saveDiagramAs()
      case "close" => RingDisplay.windowCount -= 1
        this.ringFrame.dispose()
      case "quit" => System.exit(0)
      case "chooseD" => this.chooseDiscriminant()
      case "incrD" => this.incrementDiscriminant()
      case "decrD" => this.decrementDiscriminant()
      case "prevD" => this.previousDiscriminant()
      case "nextD" => this.nextDiscriminant()
      case "copyReadouts" => this.copyReadoutsToClipboard()
      case "copyDiagram" => this.copyDiagramToClipboard()
      case "zoomIn" => this.zoomIn()
      case "zoomOut" => this.zoomOut()
      case "decrZoomStep" => this.decreaseZoomStep()
      case "incrZoomStep" => this.increaseZoomStep()
      case "decrDotRadius" => this.decreaseDotRadius()
      case "incrDotRadius" => this.increaseDotRadius()
      case "defaultView" => println("PLACEHOLDER") // TODO: Write default view reset confirm
      case "toggleTheta" => this.toggleThetaNotation()
      case "toggleReadouts" => this.toggleReadOutsEnabled()
      case "showUserManual" => this.showUserManual()
      case "about" => this.showAboutBoxMessage()
      case _ => val excMsg = "Command \"" + command + "\" in ActionEvent not recognized"
        throw new RuntimeException(excMsg)
    }
  }

  protected def setRing(ring: IntRing): Unit = {
    this.diagramRing = ring
    this.setPixelsPerBasicImaginaryInterval()
  }

  private[this] def makeFileMenu: JMenu = {
    val menu = new JMenu("File")
    menu.setMnemonic(KeyEvent.VK_F)
    menu.getAccessibleContext.setAccessibleDescription("Menu for file operations")
    var accDescr = "Save currently displayed diagram to a PNG file"
    var accelerator = KeyStroke.getKeyStroke(KeyEvent.VK_S, RingDisplay.maskCtrlCommand + Event.SHIFT_MASK)
    var menuItem = RingDisplay.makeMenuItem("Save diagram as...", accDescr, "saveDiagramAs", accelerator, this)
    menu.add(menuItem)
    menu.addSeparator()
    accDescr = "Close the window"
    accelerator = KeyStroke.getKeyStroke(KeyEvent.VK_W, RingDisplay.maskCtrlCommand)
    menuItem = RingDisplay.makeMenuItem("Close", accDescr, "close", accelerator, this)
    menu.add(menuItem)
    if (!RingDisplay.macOSFlag) {
      accDescr = "Exit the program"
      accelerator = KeyStroke.getKeyStroke(KeyEvent.VK_Q, RingDisplay.maskCtrlCommand)
      menuItem = RingDisplay.makeMenuItem("Quit", accDescr, "quit", accelerator, this)
      menu.add(menuItem)
    }
    menu
  }

  private[this] def makeEditMenu: JMenu = {
    val menu = new JMenu("Edit")
    menu.setMnemonic(KeyEvent.VK_E)
    menu.getAccessibleContext.setAccessibleDescription("Menu to change certain parameters")
    if (this.includeRingChoice) {
      val accDescr = "Let user enter new choice for ring discriminant"
      val accelerator = KeyStroke.getKeyStroke(KeyEvent.VK_D, RingDisplay.maskCtrlCommand)
      val menuItem = RingDisplay.makeMenuItem("Choose parameter d...", accDescr, "chooseD", accelerator, this)
      menu.add(menuItem)
      menu.addSeparator()
      menu.add(this.increaseDMenuItem)
      menu.add(this.decreaseDMenuItem)
    }
    var accDescr = "Copy the readouts (integer, trace, norm, polynomial) to the clipboard"
    var accelerator = KeyStroke.getKeyStroke(KeyEvent.VK_C, RingDisplay.maskCtrlCommand + Event.SHIFT_MASK)
    var menuItem = RingDisplay.makeMenuItem("Copy readouts to clipboard", accDescr, "copyReadouts", accelerator, this)
    menu.add(menuItem)
    accDescr = "Copy the currently displayed diagram to the clipboard so that it's accessible to other applications"
    accelerator = KeyStroke.getKeyStroke(KeyEvent.VK_C, RingDisplay.maskCtrlCommand + Event.ALT_MASK)
    menuItem = RingDisplay.makeMenuItem("Copy diagram to clipboard", accDescr, "copyDiagram", accelerator, this)
    menu.add(menuItem)
    // PLACEHOLDER FOR PREFERENCES MENU
    menu
  }

  private[this] def makeViewMenu: JMenu = {
    val menu = new JMenu("View")
    menu.setMnemonic(KeyEvent.VK_V)
    menu.getAccessibleContext.setAccessibleDescription("Menu to zoom in or zoom out")
    if (this.includeRingChoice) {
      this.prevDMenuItem.setEnabled(false)
      menu.add(this.prevDMenuItem)
      this.nextDMenuItem.setEnabled(false)
      menu.add(this.nextDMenuItem)
      menu.addSeparator()
    }
    if (this.pixelsPerUnitInterval > (RingDisplay.MAXIMUM_PIXELS_PER_UNIT_INTERVAL - this.zoomStep)) {
      this.zoomInMenuItem.setEnabled(false)
    }
    menu.add(this.zoomInMenuItem)
    if (this.pixelsPerUnitInterval < (RingDisplay.MINIMUM_PIXELS_PER_UNIT_INTERVAL + this.zoomStep)) {
      this.zoomOutMenuItem.setEnabled(false)
    }
    menu.add(this.zoomOutMenuItem)
    menu.addSeparator()
    if (this.zoomStep == RingDisplay.MINIMUM_ZOOM_STEP) {
      this.decreaseZoomStepMenuItem.setEnabled(false)
    }
    menu.add(this.decreaseZoomStepMenuItem)
    if (this.zoomStep == RingDisplay.MAXIMUM_ZOOM_STEP) {
      this.increaseZoomStepMenuItem.setEnabled(false)
    }
    menu.add(this.increaseZoomStepMenuItem)
    menu.addSeparator()
    if (this.dotRadius == RingDisplay.MINIMUM_DOT_RADIUS) {
      this.decreaseDotRadiusMenuItem.setEnabled(false)
    }
    menu.add(this.decreaseDotRadiusMenuItem)
    if (this.dotRadius == RingDisplay.MAXIMUM_DOT_RADIUS) {
      this.increaseDotRadiusMenuItem.setEnabled(false)
    }
    menu.add(this.increaseDotRadiusMenuItem)
    menu.addSeparator()
    var accDescr = "Reset defaults for zoom level, zoom interval and dot radius"
    var accelerator = KeyStroke.getKeyStroke(KeyEvent.VK_F7, 0)
    val menuItem = RingDisplay.makeMenuItem("Reset view defaults", accDescr, "defaultView", accelerator, this)
    menu.add(menuItem)
    menu.addSeparator()
    if (this.includeThetaToggle) {
      accDescr = "Toggle whether theta notation is used or not in the integer readout."
      this.preferThetaNotationMenuItem.getAccessibleContext.setAccessibleDescription(accDescr)
      this.preferThetaNotationMenuItem.setActionCommand("toggleTheta")
      accelerator = KeyStroke.getKeyStroke(KeyEvent.VK_T, 0)
      this.preferThetaNotationMenuItem.setAccelerator(accelerator)
      this.preferThetaNotationMenuItem.addActionListener(this)
      menu.add(this.preferThetaNotationMenuItem)
    }
    if (this.includeReadoutsUpdate) {
      accDescr = "Toggle whether the trace, norm and polynomial readouts are updated."
      this.toggleReadOutsEnabledMenuItem.getAccessibleContext.setAccessibleDescription(accDescr)
      this.toggleReadOutsEnabledMenuItem.setActionCommand("toggleReadOuts")
      accelerator = if (RingDisplay.macOSFlag) {
        KeyStroke.getKeyStroke(KeyEvent.VK_R, 0)
      } else {
        KeyStroke.getKeyStroke(KeyEvent.VK_F2, 0)
      }
      this.toggleReadOutsEnabledMenuItem.setAccelerator(accelerator)
      this.toggleReadOutsEnabledMenuItem.addActionListener(this)
      menu.add(this.toggleReadOutsEnabledMenuItem)
    }
    menu
  }

  private[this] def makeHelpMenu: JMenu = {
    val menu = new JMenu("Help")
    menu.setMnemonic(KeyEvent.VK_H)
    menu.getAccessibleContext.setAccessibleDescription("Menu to provide help and documentation")
    var menuItem = new JMenuItem("User Manual...")
    var accDescr = "Use default Web browser to show user manual"
    menuItem.getAccessibleContext.setAccessibleDescription(accDescr)
    menuItem.setActionCommand("showUserManual")
    menuItem.addActionListener(this)
    menu.add(menuItem)
    menuItem = new JMenuItem("About...")
    accDescr = "Display information about this program"
    menuItem.getAccessibleContext.setAccessibleDescription(accDescr)
    menuItem.setActionCommand("about")
    menuItem.addActionListener(this)
    menu.add(menuItem)
    menu
  }

  private[this] def setUpMenuBar(): JMenuBar = {
    val menuBar = new JMenuBar()
    menuBar.add(this.makeFileMenu)
    menuBar.add(this.makeEditMenu)
    menuBar.add(this.makeViewMenu)
    menuBar.add(this.makeHelpMenu)
    menuBar
  }

  private[this] def setUpReadOuts(): JPanel = {
    val readOutsPane = new JPanel()
    this.algIntReadOut.setText("0")
    this.algIntReadOut.setEditable(false)
    readOutsPane.add(this.algIntReadOut)
    readOutsPane.add(new JLabel("Trace: "))
    this.algIntTraceReadOut.setText("0")
    this.algIntTraceReadOut.setEditable(false)
    readOutsPane.add(this.algIntTraceReadOut)
    readOutsPane.add(new JLabel("Norm: "))
    this.algIntNormReadOut.setText("0")
    this.algIntNormReadOut.setEditable(false)
    readOutsPane.add(this.algIntNormReadOut)
    readOutsPane.add(new JLabel("Polynomial: "))
    this.algIntPolReadOut.setText("x")
    this.algIntPolReadOut.setEditable(false)
    readOutsPane.add(this.algIntPolReadOut)
    readOutsPane
  }

  private def setUpRingFrame(): Unit = {
    if (this.alreadySetUp) {
      val excMsg = this.initialTitle + " already set up, no need to set up again"
      throw new RuntimeException(excMsg)
    }
    this.ringFrame.setJMenuBar(this.setUpMenuBar())
    this.setBackground(this.backgroundColor)
    this.setPreferredSize(new Dimension(this.ringCanvasHorizMax, this.ringCanvasVerticMax))
    this.ringFrame.add(this, BorderLayout.CENTER)
    this.ringFrame.add(this.setUpReadOuts(), BorderLayout.PAGE_END)
    this.ringFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    this.ringFrame.pack()
    this.ringFrame.setVisible(true)
    this.alreadySetUp = true
  }

  def startRingDisplay(): Unit = {
    this.setUpRingFrame()
    RingDisplay.windowCount += 1
  }

}

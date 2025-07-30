package clipboardops

import java.awt.datatransfer.{Clipboard, DataFlavor, StringSelection,
  UnsupportedFlavorException}
import java.awt.Graphics
import java.awt.image.BufferedImage
import java.io.IOException

import org.junit.jupiter.api.{AfterAll, AfterEach, BeforeAll, BeforeEach, Test}
import org.junit.jupiter.api.Assertions._

object ImageSelectionTest {
  private val imgWindow = new TestImagePanel
  private val sysClip: Clipboard = imgWindow.getToolkit.getSystemClipboard
  private var strSel = new StringSelection("")
  private val img = new BufferedImage(TestImagePanel.PANEL_WIDTH,
    TestImagePanel.PANEL_HEIGHT, BufferedImage.TYPE_INT_RGB)
  private val graph: Graphics = img.getGraphics
  private val imgSel = new ImageSelection(img)

  def reportClip(): Unit = {
    val currFlavors = sysClip.getAvailableDataFlavors
    for (flavor <- currFlavors) {
      print("* " + flavor.toString)
      if (flavor.equals(DataFlavor.stringFlavor)) {
        try {
          val fromClip = sysClip.getData(flavor)
          print(" --> \"" + fromClip.toString + "\"")
        } catch {
          case ufe: UnsupportedFlavorException =>
            println("UnsupportedFlavorException for " + flavor.toString)
            println("\"" + ufe.getMessage + "\"")
          case ioe: IOException =>
            println("IOException trying to read text from the clipboard")
            println("\"" + ioe.getMessage + "\"")
        }
      }
      println()
    }
    println()
  }

  @BeforeAll def setUpClass(): Unit = {
    println("Prior to test set up, clipboard has the following data flavors:")
    reportClip()
    val initMsgClip = "This message was placed by setUpClass"
    strSel = new StringSelection(initMsgClip)
    sysClip.setContents(strSel, strSel)
    imgWindow.paint(graph)
  }

  @AfterAll def tearDownClass(): Unit = {
    println("After running the tests, the clipboard has the following data flavors:")
    reportClip()
    imgWindow.closePanel()
  }

}

class ImageSelectionTest {

  @BeforeEach def setUp(): Unit = ImageSelectionTest.sysClip
    .setContents(ImageSelectionTest.imgSel, ImageSelectionTest.imgSel)

  @Test def testGetTransferDataFlavors(): Unit = {
    println("getTransferDataFlavors")
    var msg = "DataFlavors array should only have one DataFlavor"
    val flavors = ImageSelectionTest.imgSel.getTransferDataFlavors
    assertEquals(1, flavors.length, msg)
    msg = "And that DataFlavor should be DataFlavor.imageFlavor"
    assertEquals(DataFlavor.imageFlavor, flavors(0), msg)
  }

  @Test def testIsDataFlavorSupported(): Unit = {
    println("isDataFlavorSupported")
    var currFlavor = DataFlavor.imageFlavor
    var msg = currFlavor.toString + " should be supported"
    assert(ImageSelectionTest.imgSel.isDataFlavorSupported(currFlavor), msg)
    currFlavor = DataFlavor.allHtmlFlavor
    msg = currFlavor.toString + " should not be supported"
    assert(!ImageSelectionTest.imgSel.isDataFlavorSupported(currFlavor), msg)
    currFlavor = DataFlavor.fragmentHtmlFlavor
    msg = currFlavor.toString + " should not be supported"
    assert(!ImageSelectionTest.imgSel.isDataFlavorSupported(currFlavor), msg)
    currFlavor = DataFlavor.javaFileListFlavor
    msg = currFlavor.toString + " should not be supported"
    assert(!ImageSelectionTest.imgSel.isDataFlavorSupported(currFlavor), msg)
    currFlavor = DataFlavor.selectionHtmlFlavor
    msg = currFlavor.toString + " should not be supported"
    assert(!ImageSelectionTest.imgSel.isDataFlavorSupported(currFlavor), msg)
    currFlavor = DataFlavor.stringFlavor
    msg = currFlavor.toString + " should not be supported"
    assert(!ImageSelectionTest.imgSel.isDataFlavorSupported(currFlavor), msg)
  }

  @Test def testGetTransferData(): Unit = {
    println("getTransferData")
    try {
      val data = ImageSelectionTest.imgSel
        .getTransferData(DataFlavor.imageFlavor)
      assertEquals(ImageSelectionTest.img, data)
    } catch {
      case e: Exception =>
        val msg = e.getClass.getName +
          " should not have occurred with correct DataFlavor"
        fail(msg)
    }
  }

  @Test def testGetTransferDataWrongFlavor(): Unit = {
    val flavor = DataFlavor.selectionHtmlFlavor
    val exc = assertThrows(classOf[UnsupportedFlavorException], () => {
      val data = ImageSelectionTest.imgSel.getTransferData(flavor)
      println("Trying to use " + flavor.toString
        + " to get ImageSelection transfer data should not have given data "
        + data.toString)
    })
    val excMsg = exc.getMessage
    assert(excMsg != null, "Message should not be null")
    println("\"" + excMsg + "\"")
  }

  @Test def testLostOwnership(): Unit = {
    println("lostOwnership")
    fail("Haven't written test yet")
  }

  @Test def testHasOwnership(): Unit = {
    println("hasOwnership")
    var msg = "ImageSelection should have ownership of the clipboard"
    assert(ImageSelectionTest.imgSel.hasOwnership, msg)
    val testClipMsg =
      "This message was placed by ImageSelectionTest.testHasOwnership()"
    ImageSelectionTest.strSel = new StringSelection(testClipMsg)
    ImageSelectionTest.sysClip.setContents(ImageSelectionTest.strSel,
      ImageSelectionTest.strSel)
    msg = "StringSelection, not ImageSelection, should have ownership of the clipboard"
    assert(!ImageSelectionTest.imgSel.hasOwnership, msg)
  }

  @AfterEach def tearDown(): Unit = {
    println("Right now the clipboard has the following data flavors:")
    ImageSelectionTest.reportClip()
  }

}

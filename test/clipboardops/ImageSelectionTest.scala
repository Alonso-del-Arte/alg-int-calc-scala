package clipboardops

import java.awt.datatransfer.{Clipboard, DataFlavor, StringSelection, UnsupportedFlavorException}
import java.awt.Graphics
import java.awt.image.BufferedImage
import java.io.IOException

import org.junit.{After, AfterClass, Before, BeforeClass, Ignore}
import org.junit.jupiter.api.{AfterAll, AfterEach, BeforeAll, BeforeEach, Test}
import org.junit.jupiter.api.Assertions._

object ImageSelectionTest {
  val imgWindow = new TestImagePanel
  val sysClip: Clipboard = imgWindow.getToolkit.getSystemClipboard
  var strSel = new StringSelection("")
  var img = new BufferedImage(TestImagePanel.PANEL_WIDTH, TestImagePanel.PANEL_HEIGHT, BufferedImage.TYPE_INT_RGB)
  val graph: Graphics = img.getGraphics
  var imgSel = new ImageSelection(img)

  def reportClip(): Unit = {
    val currFlavors = sysClip.getAvailableDataFlavors
    for (flavor <- currFlavors) {
      print("* " + flavor.toString)
      if (flavor.equals(DataFlavor.stringFlavor)) {
        try {
          val fromClip = sysClip.getData(flavor)
          print(" --> \"" + fromClip + "\"")
        } catch {
          case ufe: UnsupportedFlavorException => println("Encountered UnsupportedFlavorException for DataFlavor.stringFlavor")
            println("\"" + ufe.getMessage + "\"")
          case ioe: IOException => println("Encountered IOException trying to read text from the clipboard")
            println("\"" + ioe.getMessage + "\"")
        }
      }
      println()
    }
    println()
  }

  @BeforeClass def setUpClass(): Unit = {
    println("Prior to test set up, clipboard has the following data flavors:")
    reportClip()
    val initMsgClip = "This message was placed by the ImageSelectionTest constructor"
    strSel = new StringSelection(initMsgClip)
    sysClip.setContents(strSel, strSel)
    imgWindow.paint(graph)
  }

  @AfterClass def tearDownClass(): Unit = {
    println("After running the tests, the clipboard has the following data flavors:")
    reportClip()
    imgWindow.closePanel()
  }

}

class ImageSelectionTest {

  @Before def setUp(): Unit = ImageSelectionTest.sysClip.setContents(ImageSelectionTest.imgSel, ImageSelectionTest.imgSel)

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
    assertTrue(ImageSelectionTest.imgSel.isDataFlavorSupported(currFlavor), msg)
    currFlavor = DataFlavor.allHtmlFlavor
    msg = currFlavor.toString + " should not be supported"
    assertFalse(ImageSelectionTest.imgSel.isDataFlavorSupported(currFlavor), msg)
    currFlavor = DataFlavor.fragmentHtmlFlavor
    msg = currFlavor.toString + " should not be supported"
    assertFalse(ImageSelectionTest.imgSel.isDataFlavorSupported(currFlavor), msg)
    currFlavor = DataFlavor.javaFileListFlavor
    msg = currFlavor.toString + " should not be supported"
    assertFalse(ImageSelectionTest.imgSel.isDataFlavorSupported(currFlavor), msg)
    currFlavor = DataFlavor.selectionHtmlFlavor
    msg = currFlavor.toString + " should not be supported"
    assertFalse(ImageSelectionTest.imgSel.isDataFlavorSupported(currFlavor), msg)
    currFlavor = DataFlavor.stringFlavor
    msg = currFlavor.toString + " should not be supported"
    assertFalse(ImageSelectionTest.imgSel.isDataFlavorSupported(currFlavor), msg)
  }

  @Test def testGetTransferData(): Unit = {
    println("getTransferData")
    try {
      val data = ImageSelectionTest.imgSel.getTransferData(DataFlavor.imageFlavor)
      assertEquals(ImageSelectionTest.img, data)
    } catch {
      case e: Exception => val failMsg = e.getClass.getName + " should not have occurred trying to access ImageSelection transfer data with correct DataFlavor"
        println(failMsg)
        println("\"" + e.getMessage + "\"")
        fail(failMsg)
    }
  }

  @Test def testGetTransferDataWrongFlavor(): Unit = {
    val flavor = DataFlavor.selectionHtmlFlavor
    try {
      val data = ImageSelectionTest.imgSel.getTransferData(flavor)
      val failMsg = "Trying to use " + flavor.toString + " to get ImageSelection transfer data should have caused an exception, not given data " + data.toString
      fail(failMsg)
    } catch {
      case ufe: UnsupportedFlavorException => println("Trying to use " + flavor.toString + " to get ImageSelection transfer data correctly caused UnsupportedFlavorException")
        println("\"" + ufe.getMessage + "\"")
      case e: Exception => val failMsg = e.getClass.getName + " is the wrong exception to throw for trying to access ImageSelection transfer data with wrong DataFlavor"
        println(failMsg)
        println("\"" + e.getMessage + "\"")
        fail(failMsg)
    }
  }

  @Ignore @Test def testLostOwnership(): Unit = {
    println("lostOwnership")
    fail("Haven't written test yet")
  }

  @Test def testHasOwnership(): Unit = {
    println("hasOwnership")
    var msg = "ImageSelection should have ownership of the clipboard"
    assertTrue(ImageSelectionTest.imgSel.hasOwnership, msg)
    val testClipMsg = "This message was placed by ImageSelectionTest.testHasOwnership()"
    ImageSelectionTest.strSel = new StringSelection(testClipMsg)
    ImageSelectionTest.sysClip.setContents(ImageSelectionTest.strSel, ImageSelectionTest.strSel)
    msg = "StringSelection, not ImageSelection, should have ownership of the clipboard"
    assertFalse(ImageSelectionTest.imgSel.hasOwnership, msg)
  }

  @After def tearDown(): Unit = {
    println("Right now the clipboard has the following data flavors:")
    ImageSelectionTest.reportClip()
  }

}

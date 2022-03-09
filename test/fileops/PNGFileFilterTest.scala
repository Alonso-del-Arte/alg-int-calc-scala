package fileops

import java.io.File

import org.junit.Test
import org.junit.Assert._

class PNGFileFilterTest {

  private val filter = new PNGFileFilter

  @Test def testAccept(): Unit = {
    println("accept")
    var file = new File("image.png")
    var assertionMessage = "PNGFileFilter should accept " + file.getName
    println(assertionMessage)
    assertTrue(assertionMessage, filter.accept(file))
    file = new File("image.PNG")
    assertionMessage = "PNGFileFilter should accept " + file.getName
    println(assertionMessage)
    assertTrue(assertionMessage, filter.accept(file))
    file = new File("image.jpg")
    assertionMessage = "PNGFileFilter should reject " + file.getName
    println(assertionMessage)
    assertFalse(assertionMessage, filter.accept(file))
    file = new File("document.doc")
    assertionMessage = "PNGFileFilter should reject " + file.getName
    println(assertionMessage)
    assertFalse(assertionMessage, filter.accept(file))
    val homeDir = System.getProperty("user.home")
    val dir = new File(homeDir)
    assertionMessage = "PNGFileFilter should accept directory " + homeDir
    println(assertionMessage)
    assertTrue(assertionMessage, filter.accept(dir))
  }

  @Test def testGetDescription(): Unit = {
    println("getDescription")
    val description = filter.getDescription
    println("PNGFileFilter description is \"" + description + "\"")
    var assertionMessage = "Filter description should include \"Portable Network Graphics\""
    assertTrue(assertionMessage, description.contains("Portable Network Graphics"))
    assertionMessage = "Filter description should include \"png\" or \"PNG\""
    assertTrue(assertionMessage, description.toLowerCase.contains("png"))
  }

}

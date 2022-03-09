package fileops

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class FileChooserWithOverwriteGuardTest {

  class MockFileChooser(val response: Int)
    extends FileChooserWithOverwriteGuard {

    override def getConfirmationResponse(filename: String): Int = this.response

  }

  class ThreadableMockFileChooser(response: Int)
    extends MockFileChooser(response)

}

package fileops

import javax.swing.{JFileChooser, JOptionPane}

class FileChooserWithOverwriteGuard extends JFileChooser {
  private val msg = "Do you want to overwrite the existing file?"

  private[fileops] def getConfirmationResponse(filename: String): Int = {
    JOptionPane.showConfirmDialog(this, msg, filename + " already exists",
      JOptionPane.YES_NO_CANCEL_OPTION)
  }

  // TODO: Write tests for this
  override def approveSelection(): Unit = {
    val file = this.getSelectedFile
    if (file.exists()) {
      val confResp = this.getConfirmationResponse(file.getName)
      confResp match {
        case JOptionPane.YES_OPTION => this.cancelSelection()
        case JOptionPane.NO_OPTION => super.approveSelection()
        case _ => super.approveSelection()
      }
    }
    super.approveSelection()
  }

}

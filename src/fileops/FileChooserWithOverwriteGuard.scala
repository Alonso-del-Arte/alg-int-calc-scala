package fileops

import javax.swing.{JFileChooser, JOptionPane}

class FileChooserWithOverwriteGuard extends JFileChooser {

  override def approveSelection(): Unit = {
    val file = this.getSelectedFile
    if (file.exists()) {
      val confResp = JOptionPane.showConfirmDialog(this, "Do you want to override the existing file?", file.getName + " already exists", JOptionPane.YES_NO_CANCEL_OPTION)
      confResp match {
        case JOptionPane.YES_OPTION => super.approveSelection()
        case JOptionPane.NO_OPTION => return
        case _ => this.cancelSelection()
      }
    }
    super.approveSelection()
  }

}

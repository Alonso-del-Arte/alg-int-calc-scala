package fileops

import java.io.File
import javax.swing.filechooser.FileFilter

class PNGFileFilter extends FileFilter {

  override def accept(f: File): Boolean = {
    if (f.isDirectory) true else f.getName.toLowerCase.endsWith(".png")
  }

  override def getDescription: String = "Portable Network Graphics files (*.png)"

}

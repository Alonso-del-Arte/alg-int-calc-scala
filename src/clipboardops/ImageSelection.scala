package clipboardops

import java.awt.datatransfer.{Clipboard, ClipboardOwner, DataFlavor, Transferable, UnsupportedFlavorException}
import java.awt.Image

object ImageSelection {

  val FLAVOR: Array[DataFlavor] = Array(DataFlavor.imageFlavor)

}

class ImageSelection(image: Image) extends ClipboardOwner with Transferable {
  private val img = image
  private[this] var clipboardOwnershipFlag = false

  override def getTransferDataFlavors: Array[DataFlavor] = ImageSelection.FLAVOR

  override def isDataFlavorSupported(flavor: DataFlavor): Boolean = flavor.equals(ImageSelection.FLAVOR(0))

  override def getTransferData(flavor: DataFlavor): AnyRef = {
    if (flavor.equals(DataFlavor.imageFlavor)) {
      this.clipboardOwnershipFlag = true
      this.img
    } else {
      throw new UnsupportedFlavorException(flavor)
    }
  }

  override def lostOwnership(clipboard: Clipboard, contents: Transferable): Unit = this.clipboardOwnershipFlag = false

  def hasOwnership: Boolean = this.clipboardOwnershipFlag

}

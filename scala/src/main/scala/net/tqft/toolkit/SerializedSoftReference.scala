package net.tqft.toolkit

import java.io._
import java.lang.ref._

class SerializedSoftReference[A](a: A, _classLoader: ClassLoader) extends Object with Logging {

  def classLoader = if(_classLoader == null) {
    this.getClass.getClassLoader
  } else {
    _classLoader
  }
  
  private class OOS(os: OutputStream) extends ObjectOutputStream(os)
  private class OIS(is: InputStream) extends ObjectInputStream(is) {
    override protected def resolveClass(objectStreamClass: ObjectStreamClass): Class[_] = {
      Class.forName(objectStreamClass.getName(), true, classLoader);
    }
  }

  val file = File.createTempFile("SerializedSoftReference", "")
  //	info("Temporary file: " + file.getAbsolutePath)
  file.deleteOnExit

  {
    val oos = new OOS(new FileOutputStream(file))
    oos.writeObject(a)
    oos.close()
  }

  var ref = new SoftReference(a)

  def release {
    file.delete
  }

  def apply() = {
    ref.get match {
      case null => {
        info("Cache hit!")
        loadObject
      }
      case b => b
    }
  }

  private[this] def loadObject: A = {
    try {
      if (file.exists) {
        val ois = new OIS(new FileInputStream(file))
        val b = ois.readObject.asInstanceOf[A]
        ref = new SoftReference(b)
        b
      } else {
        throw new FileNotFoundException(file.getAbsolutePath)
      }
    } catch {
      case e: Exception =>
        error("Something went wrong while trying to load serialized soft reference: ", e)
        throw e
    }
  }
}
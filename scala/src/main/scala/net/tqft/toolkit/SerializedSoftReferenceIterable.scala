package net.tqft.toolkit

import java.io._
import java.lang.ref._

object SerializedSoftReferenceIterable {
  import scala.collection.mutable.{ ListBuffer, Builder }

  def newBuilder[A <% Serializable](classLoader: ClassLoader): Builder[A, SerializedSoftReferenceIterable[A]] = new SerializedSoftReferenceIterableBuilder[A](classLoader)

  private class SerializedSoftReferenceIterableBuilder[A](classLoader: ClassLoader) extends Builder[A, SerializedSoftReferenceIterable[A]] {
    private[this] val builder = new ListBuffer[A]

    override def +=(elem: A) = {
      builder += elem
      this
    }

    override def clear {
      builder.clear
    }

    override def result = {
      new SerializedSoftReferenceIterable(builder.result, classLoader)
    }

  }
}

class SerializedSoftReferenceIterable[A](underlying: Iterable[A], _classLoader: ClassLoader) extends Iterable[A] with Logging {

  def classLoader = if (_classLoader == null) {
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
    oos.writeObject(underlying)
    oos.close()
  }

  var ref = new SoftReference(underlying)

  def release {
    file.delete
  }

  def iterator() = {
    (ref.get match {
      case null => {
        info("Cache hit!")
        loadObject
      }
      case b => b
    }).iterator
  }

  private[this] def loadObject: Iterable[A] = {
    try {
      if (file.exists) {
        val ois = new OIS(new FileInputStream(file))
        val b = ois.readObject.asInstanceOf[Iterable[A]]
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
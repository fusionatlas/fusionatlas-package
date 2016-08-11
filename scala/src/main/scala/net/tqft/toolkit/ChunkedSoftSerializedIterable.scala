package net.tqft.toolkit

import java.io.Serializable

//object ChunkedSoftSerializedIterable {
//	import scala.collection.mutable.Builder
//	
//	def newBuilder[A <% Serializable](classLoader: ClassLoader): Builder[A, ChunkedSoftSerializedIterable[A]] = new ChunkedSoftSerializedIterableBuilder[A](classLoader = classLoader)
//}
//
//class ChunkedSoftSerializedIterable[A <% Serializable](list: List[SerializedSoftReference[List[A]]], chunkSize: Int) extends Iterable[A] {
//	def this(underlying: Iterable[A], chunkSize: Int = 1000, classLoader: ClassLoader) = {
//		this(for(chunk <- underlying.grouped(chunkSize).toList) yield {
//			new SerializedSoftReference(chunk.toList, _classLoader = classLoader)
//		}, chunkSize)
//	}
//	
//	override def size = {
//		if(list isEmpty) {
//			0
//		} else {
//			(list.size - 1) * chunkSize + list.last().size
//		}
//	}
//	
//	def release {
//		for(ssr <- list) ssr.release
//	}
//	
//	def iterator = {
//		list.iterator flatMap { _().iterator }
//	}
//	
//}
//
import scala.collection.mutable.Builder
import scala.collection.mutable.ListBuffer

//class ChunkedListBuilder[A](chunkSize: Int) extends ChunkedBuilder[A, List[A], List[List[A]]](() => new ListBuffer, new ListBuffer, chunkSize)
//
class ChunkedSoftReferencedIterableBuilder[A <: Serializable](chunkSize: Int, classLoader: ClassLoader) extends ChunkedBuilder[A, SerializedSoftReferenceIterable[A], Iterable[SerializedSoftReferenceIterable[A]]](() => SerializedSoftReferenceIterable.newBuilder[A](classLoader), new ListBuffer, chunkSize)

class ChunkedBuilder[A, CC <: Traversable[A], DD <: Traversable[CC]]
     (innerBuilder: () => Builder[A, CC], outerBuilder: Builder[CC, DD], chunkSize: Int) extends Builder[A, DD] {
         
   var totalSize = 0
	var currentChunkSize = 0
	var currentChunkBuilder = innerBuilder()
	
  def finishChunk {
		synchronized {
			outerBuilder += currentChunkBuilder.result
			currentChunkSize = 0
			currentChunkBuilder = innerBuilder()
		}
	}
	
  /** Adds a single element to the builder.
   *  @param elem the element to be added.
   *  @return the builder itself.
   */
  def +=(elem: A): this.type = {
		if(currentChunkSize >= chunkSize) {
			finishChunk
		}
		synchronized {
			totalSize = totalSize + 1
			currentChunkSize = currentChunkSize + 1
			currentChunkBuilder += elem
		}
		this
	}
  
  /** Clears the contents of this builder.
   *  After execution of this method the builder will contain no elements.
   */
  def clear() {
	  synchronized {
	 	  currentChunkSize = 0
	 	  currentChunkBuilder = innerBuilder()
	 	  outerBuilder.clear
	  }
  }

  /** Produces a collection from the added elements.
   *  The builder's contents are undefined after this operation.
   *  @return a collection containing the elements added to this builder.
   */
  def result() = {
	    finishChunk
		outerBuilder.result
	}	
    	 
}
//
//private class ChunkedSoftSerializedIterableBuilder[A <% Serializable](chunkSize: Int = 1000, classLoader: ClassLoader) extends Builder[A, ChunkedSoftSerializedIterable[A]] {
//
//	var totalSize = 0
//	var currentChunk = 0
//	var currentChunkBuilder = new ListBuffer[A]
//	var builder = new ListBuffer[SerializedSoftReference[List[A]]]
//	
//  def finishChunk {
//		synchronized {
//			builder += new SerializedSoftReference(currentChunkBuilder.result, classLoader)
//			currentChunk = 0
//			currentChunkBuilder = new ListBuffer[A]
//		}
//	}
//	
//  /** Adds a single element to the builder.
//   *  @param elem the element to be added.
//   *  @return the builder itself.
//   */
//  def +=(elem: A): this.type = {
//		if(currentChunk >= chunkSize) {
//			finishChunk
//		}
//		synchronized {
//			totalSize = totalSize + 1
//			currentChunk = currentChunk + 1
//			currentChunkBuilder += elem
//		}
//		this
//	}
//  
//  /** Clears the contents of this builder.
//   *  After execution of this method the builder will contain no elements.
//   */
//  def clear() {
//	  synchronized {
//	 	  currentChunk = 0
//	 	  currentChunkBuilder = new ListBuffer[A]
//	 	  builder = new ListBuffer[SerializedSoftReference[List[A]]]
//	  }
//  }
//
//  /** Produces a collection from the added elements.
//   *  The builder's contents are undefined after this operation.
//   *  @return a collection containing the elements added to this builder.
//   */
//  def result(): ChunkedSoftSerializedIterable[A] = {
//	    finishChunk
//		new ChunkedSoftSerializedIterable(builder.result, chunkSize)
//	}
//  
//}
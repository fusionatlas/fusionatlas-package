package org.fusionatlas.v1.odometer;

import java.util.Iterator;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

/** warning, this implementation is uselessly slow ... **/

public class ParallelCachingIterable<T> implements Iterable<T> {

  private final Iterable<T> iterable;
  private final int         cacheSize;

  public ParallelCachingIterable(Iterable<T> iterable, int cacheSize) {
    this.iterable = iterable;
    this.cacheSize = cacheSize;
  }

  public Iterator<T> iterator() {
    final Iterator<T> iterator = iterable.iterator();
    final BlockingQueue<T> queue = new LinkedBlockingQueue<T>(cacheSize);
    final Thread worker = new Thread() {
      @Override
      public void run() {
        while (iterator.hasNext()) {
          T next = iterator.next();
          while(!queue.offer(next)) {
            synchronized(queue) {
              queue.notifyAll();
            }
          }
        }
        synchronized(queue) {
          queue.notifyAll();
        }
      }
    };

    worker.setDaemon(true);
    worker.start();

    return new Iterator<T>() {

      public boolean hasNext() {
        if (!queue.isEmpty())
          return true;
        else {
          synchronized(queue) {
            while(queue.isEmpty() && worker.isAlive()) {
              try {
                queue.wait();
              } catch (InterruptedException e) {
                e.printStackTrace();
              }
            }
          }
          return !queue.isEmpty();
        }
      }

      public T next() {
        return queue.poll();
      }

      public void remove() {
        throw new UnsupportedOperationException();
      }

    };
  }

}

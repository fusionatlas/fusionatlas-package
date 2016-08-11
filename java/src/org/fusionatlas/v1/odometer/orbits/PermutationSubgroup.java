package org.fusionatlas.v1.odometer.orbits;

import java.util.Iterator;

public class PermutationSubgroup {

  public PermutationSubgroup(int[][] orbits) {
    this.orbits = orbits;
  }

  final int[][] orbits;

  public <T> Iterable<T[]> generateOrbitOf(final T[] t) {
    return new Iterable<T[]>() {

      public Iterator<T[]> iterator() {
        return new Iterator<T[]>() {
          int[][] indexes = new int[orbits.length][];
          T[]     next;

          {
            for (int i = 0; i < orbits.length; ++i) {
              indexes[i] = new int[orbits[i].length - 1];
            }
            next = fill();
          }

          private T[] fill() {
            T[] result = t.clone();
            for (int i = 0; i < result.length; ++i) {
              result[i] = null;
            }
            for (int i = 0; i < indexes.length; ++i) {
              for (int j = 0; j < indexes[i].length; ++j) {
                System.out.print(indexes[i][j]);
              }
              System.out.print(" ");
            }
            System.out.println();
            int m = 0;
            for (int i = 0; i < indexes.length; ++i) {
              for (int j = indexes[i].length - 1; j >= 0; --j) {
                int l = indexes[i][j];
                for (int k = indexes[i].length - 1; k > j; --k) {
                  if (indexes[i][k] <= indexes[i][j])
                    ++l;
                }
                result[orbits[i][l]] = t[m++];
              }
              int l = 0;
              for (int k = indexes[i].length - 1; k >= 0; --k) {
                if (indexes[i][k] == 0)
                  ++l;
              }
              result[orbits[i][l]] = t[m++];
            }
            return result;
          }

          private void advance() {
            ++indexes[0][0];
          }

          private boolean invalid() {
            for (int i = 0; i < indexes.length; ++i) {
              for (int j = 0; j < indexes[i].length; ++j) {
                if (indexes[i][j] > j + 1)
                  return true;
              }
            }
            return false;
          }

          private boolean carry() {
            for (int i = 0; i < indexes.length; ++i) {
              for (int j = 0; j < indexes[i].length; ++j) {
                if (indexes[i][j] != 0) {
                  indexes[i][j] = 0;
                  ++j;
                  if (j == indexes[i].length) {
                    j = 0;
                    ++i;
                  }
                  if (i == indexes.length) {
                    return false;
                  } else {
                    ++indexes[i][j];
                    return true;
                  }
                }
              }
            }
            assert false;
            return false;
          }

          public boolean hasNext() {
            return next != null;
          }

          public T[] next() {
            T[] result = next;
            advance();
            while (invalid()) {
              if (!carry()) {
                next = null;
                break;
              }
            }
            if (next != null)
              next = fill();
            return result;
          }

          public void remove() {
            throw new UnsupportedOperationException();
          }

        };
      }

    };
  }
}

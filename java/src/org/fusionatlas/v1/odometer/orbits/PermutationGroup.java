package org.fusionatlas.v1.odometer.orbits;

public class PermutationGroup extends PermutationSubgroup {
  
  public PermutationGroup(int n) {
    super(new int[][] { range(n) });
  }
  
  private static int[] range(int n) {
    int[] result = new int[n];
    for(int i = 0; i < n; ++i) {
      result[i] = i;
    }
    return result;
  }
  
}

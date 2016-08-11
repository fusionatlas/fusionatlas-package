package org.fusionatlas.v1.odometer;


import static org.junit.Assert.assertEquals;

import java.io.IOException;

import net.tqft.iterables.Iterables;

import org.fusionatlas.v1.odometer.GradedBigraph;
import org.junit.Before;
import org.junit.Test;

public class OdometerTest {

  @Before
  public void setUp() throws Exception {
  }

  static private int[][] m1 = new int[][] { { 1 } };
  static private int[][] m2 = new int[][] { { 1 }, { 1 } };
  static private int[][] m3 = new int[][] { { 1, 0 }, { 0, 1 } };
  static private int[][] m4 = new int[][] { { 1, 1 } };

  static private GradedBigraph d4 = new GradedBigraph(m1, m2);
  static private GradedBigraph d5 = new GradedBigraph(m1, m1, m2);
  static private GradedBigraph hex = new GradedBigraph(m1, m1, m1, m2, m3, m4);
  static private GradedBigraph trivalent122 = new GradedBigraph(m1, m2, m3);
  static private GradedBigraph mostHaagerup = new GradedBigraph(m1, m1, m1, m2, m3);

  @Test
  public void testOdometer1() throws IOException {
    assertEquals(18, Iterables.sizeOf(d4.findExtensionsUpToRank(3., 3, 5)));
    assertEquals(30, Iterables.sizeOf(d5.findExtensionsUpToRank(2.5, 4, 12)));
    assertEquals(17, Iterables.sizeOf(hex.findExtensionsUpToRank(3.75, 5, -1)));
    assertEquals(247, Iterables.sizeOf(trivalent122.findExtensionsUpToRank(2.88, 121, 121)));
    assertEquals(18, Iterables.sizeOf(mostHaagerup.findExtensionsUpToRankAndDepth(Math.sqrt(5), -1, 6)));    
  }
  
}

package org.fusionatlas.v1.odometer;


import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.fusionatlas.v1.odometer.Matrix;
import org.fusionatlas.v1.odometer.SymmetricTridiagonalMatrix;
import org.junit.Before;
import org.junit.Test;

public class EigenvalueTest {

  @Before
  public void setUp() throws Exception {
  }

  @Test
  public void testEigenvalueBound1() throws IOException {
    int[][] m1 = new int[][] {{1,1 },{1,1}};
    int[][] m2 = new int[][] {{1,0 },{1,0}};
    
    Matrix m = new SymmetricTridiagonalMatrix(m1, m2);
    assertTrue(m.lowerBoundForGreatestEigenvalue() < 2.28825);
    assertTrue(m.lowerBoundForGreatestEigenvalue() > 2.28824);
  }
  
  @Test
  public void testEigenvalueBound2() throws IOException {
    int[][] m1 = new int[][] {{1}};
    int[][] m2 = new int[][] {{1},{1}};
    int[][] m3 = new int[][] {{1,0},{0,1}};
    
    Matrix haagerup = new SymmetricTridiagonalMatrix(m1, m1, m1, m2, m3, m3);
    assertTrue(haagerup.lowerBoundForGreatestEigenvalue() < 2.074314);
    assertTrue(haagerup.lowerBoundForGreatestEigenvalue() > 2.074313);
  }
  
}

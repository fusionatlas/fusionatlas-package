package org.fusionatlas.v1.odometer;


public abstract class AbstractMatrix implements Matrix {

  protected final int size;
  protected float[]     approximateFPEigenvector;

  public AbstractMatrix(int size) {
    this.size = size;
  }

  protected void resetEigenvector() {
    approximateFPEigenvector = new float[size];
    for (int i = 0; i < size; ++i) {
      approximateFPEigenvector[i] = 1;
    }
  }

  public float lowerBoundForGreatestEigenvalue() {
    improveApproximateFPEigenvector();
    return lowerBoundForGreatestEigenvalue(approximateFPEigenvector);
  }

  private void improveApproximateFPEigenvector() {
    if (approximateFPEigenvector == null)
      resetEigenvector();
    for (int j = 0; j < 10; ++j) {
      approximateFPEigenvector = multiply(approximateFPEigenvector);
    }
  }
  
  public float lowerBoundForGreatestEigenvalue(float[] vector) {
    float[] newVector = multiply(vector);
    approximateFPEigenvector = newVector;
    return (float) Math.sqrt(innerProduct(newVector, newVector) / innerProduct(vector, vector));
  }

  private static float innerProduct(float[] v1, float[] v2) {
    float r = 0;
    for (int i = 0; i < v1.length; ++i) {
      r += v1[i] * v2[i];
      assert r >= 0;
    }
    return r;
  }

}

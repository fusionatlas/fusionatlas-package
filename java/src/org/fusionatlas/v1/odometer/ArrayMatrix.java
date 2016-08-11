package org.fusionatlas.v1.odometer;


public class ArrayMatrix extends AbstractMatrix {
  
  private int[][] values;
    
  public ArrayMatrix(int[][] values) {
    super(values.length);
    this.values = values;
  }
  
  public float[] multiply(float[] vector) {
	float[] result = new float[size];
	for(int j = 0; j < size; ++j) {
		result[j] = 0;
	}
    for(int i = 0; i < size; ++i) {
      for(int j = 0; j < size; ++j) {
        result[i] += vector[j] * values[i][j];
      }
    }
    return result;
  }
  
}

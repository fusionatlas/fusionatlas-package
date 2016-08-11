package org.fusionatlas.v1.odometer;


public interface Matrix {

  public abstract float lowerBoundForGreatestEigenvalue();

  public abstract float lowerBoundForGreatestEigenvalue(float[] vector);

  public abstract float[] multiply(float[] vector);

}
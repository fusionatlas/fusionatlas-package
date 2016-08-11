/*
 * Created on Jun 25, 2007
 */
package org.fusionatlas.v1.odometer.orbits;


public class StringIterables {

	// don't allow instances -- this is a purely static class
	private StringIterables() {
	}

	private Object readResolve() {
		return null;
	}
	
	public static String concatenate(Iterable<String> strings) {
    StringBuilder sb = new StringBuilder();
    for(String string : strings) {
      sb.append(string);
    }
    return sb.toString();
  }

}

package org.fusionatlas.v1.odometer.orbits;


import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.Arrays;
import java.util.Iterator;

import org.fusionatlas.v1.odometer.orbits.PermutationGroup;
import org.junit.Before;
import org.junit.Test;

public class PermutationTest {

  @Before
  public void setUp() throws Exception {
  }

  @Test
  public void testPermutationGroup() throws IOException {
    PermutationGroup p = new PermutationGroup(4);
    Iterable<String[]> orbit = p.generateOrbitOf(new String[] { "a", "b", "c", "d" });
    Iterator<String[]> iterator = orbit.iterator();
    assertEquals("abcd", concatenate(iterator.next()));
    assertEquals("abdc", concatenate(iterator.next()));
    assertEquals("acbd", concatenate(iterator.next()));

  }
  
 private String concatenate(String[] strings) {
   return StringIterables.concatenate(Arrays.asList(strings));
 }
  
}

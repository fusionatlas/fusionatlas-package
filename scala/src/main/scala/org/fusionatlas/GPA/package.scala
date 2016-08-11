package org.fusionatlas

import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.Fraction
package object GPA {
	def ??? = throw new NoSuchMethodException
	
	type F = Polynomial[Fraction[Int]]

	type Strand = Vertex
	type ChiralStrand = Chiral[Vertex, Int]
	
	type Tangle = PlanarTangle[Strand, Int]
	type ChiralTangle = PlanarTangle[ChiralStrand, (Int, Int)]
	
	type ChiralDisk = Disk[ChiralStrand, (Int, Int)]
}
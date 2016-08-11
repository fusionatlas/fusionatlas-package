package org.fusionatlas.v1.odometer;


public class Proxy {

	private Proxy() { };

	public static String[][] findExtensionsMatching(String shortGraph,
			String[] longGraphs, double norm) {
		GradedBigraph[] graphs = new GradedBigraph[longGraphs.length];
		for(int i = 0; i < longGraphs.length; ++i) {
			graphs[i] = new GradedBigraph(longGraphs[i]);
		}
		return GradedBigraph.listOfIterablesToStringArray((new GradedBigraph(shortGraph)).findExtensionsMatching(norm, graphs));
	}
	
	public static String[] findExtensionsUpToRank(String graph, final double d,
			final int additionalRank, final int maximalLoops) {
		return GradedBigraph.iterableToStringArray(new GradedBigraph(graph).findExtensionsUpToRank(d, additionalRank, maximalLoops));
	}

	
}

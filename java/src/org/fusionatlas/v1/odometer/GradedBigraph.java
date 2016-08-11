package org.fusionatlas.v1.odometer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import net.tqft.iterables.IterableBundle;
import net.tqft.iterables.Iterables;
import net.tqft.iterables.TreeIterable;
import net.tqft.iterables.interfaces.Predicate;

public class GradedBigraph {

	@Override
	public boolean equals(Object arg0) {
		if (!(arg0 instanceof GradedBigraph)) {
			return false;
		} else {
			return adjacencyMatrix
					.equals(((GradedBigraph) arg0).adjacencyMatrix);
		}
	}

	@Override
	public int hashCode() {
		return 23875078 + adjacencyMatrix.hashCode();
	}

	private SymmetricTridiagonalMatrix adjacencyMatrix;

	private int[] monotonePathsToPenultimateDepth;
	private int[] stableLoopCounts;

	public GradedBigraph(int[][]... matrices) {
		adjacencyMatrix = new SymmetricTridiagonalMatrix(matrices);
	}

	public GradedBigraph(GradedBigraph g) {
		adjacencyMatrix = new SymmetricTridiagonalMatrix(g.adjacencyMatrix);
		monotonePathsToPenultimateDepth = g.monotonePathsToPenultimateDepth;
	}

	public GradedBigraph(GradedBigraph g, int[][] matrix) {
		adjacencyMatrix = new SymmetricTridiagonalMatrix(g.adjacencyMatrix,
				matrix);
	}

	public GradedBigraph(String gbg) {
		if (!gbg.startsWith("gbg")) {
			throw new IllegalArgumentException(
					"String didn't start with \"gbg\"");
		}

		String[] matrixStrings, rows, entries;
		matrixStrings = gbg.substring(3).split("v");
		int[][][] matrices = new int[matrixStrings.length][][];
		int i = 0, j = 0, k = 0;
		for (String matrix : matrixStrings) {
			rows = matrix.split("p");
			matrices[i] = new int[rows.length][];
			for (String row : rows) {
				entries = row.split("x");
				matrices[i][j] = new int[entries.length];
				for (String entry : entries) {
					try {
						matrices[i][j][k++] = Integer.parseInt(entry);
					} catch (NumberFormatException e) {
						throw new IllegalArgumentException(
								"Matrix entry wasn't an integer.");
					}
				}
				k = 0;
				++j;
			}
			j = 0;
			++i;
		}

		adjacencyMatrix = new SymmetricTridiagonalMatrix(matrices);
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		boolean trailingChar;
		sb.append("gbg");
		trailingChar = false;
		for (int[][] matrix : adjacencyMatrix.subDiagonalBlocks) {
			trailingChar = false;
			for (int[] row : matrix) {
				trailingChar = false;
				for (int entry : row) {
					sb.append(entry);
					sb.append("x");
					trailingChar = true;
				}
				if (trailingChar) {
					sb.deleteCharAt(sb.length() - 1);
					trailingChar = false;
				}
				sb.append("p");
				trailingChar = true;
			}
			if (trailingChar) {
				sb.deleteCharAt(sb.length() - 1);
				trailingChar = false;
			}
			sb.append("v");
			trailingChar = true;
		}
		if (trailingChar) {
			sb.deleteCharAt(sb.length() - 1);
			trailingChar = false;
		}
		return sb.toString();
	}

	public String toStringExpression() {
		StringBuilder sb = new StringBuilder();
		boolean trailingChar;
		sb.append("GradedBigraph[");
		trailingChar = false;
		for (int[][] matrix : adjacencyMatrix.subDiagonalBlocks) {
			sb.append("{");
			trailingChar = false;
			for (int[] row : matrix) {
				sb.append("{");
				trailingChar = false;
				for (int entry : row) {
					sb.append(entry);
					sb.append(",");
					trailingChar = true;
				}
				if (trailingChar) {
					sb.deleteCharAt(sb.length() - 1);
					trailingChar = false;
				}
				sb.append("},");
				trailingChar = true;
			}
			if (trailingChar) {
				sb.deleteCharAt(sb.length() - 1);
				trailingChar = false;
			}
			sb.append("},");
			trailingChar = true;
		}
		if (trailingChar) {
			sb.deleteCharAt(sb.length() - 1);
			trailingChar = false;
		}
		sb.append("]");
		return sb.toString();
	}

	public static String iterableToString(Iterable<GradedBigraph> iterable) {
		StringBuilder sb = new StringBuilder();
		boolean trailingChar;
		sb.append("{");
		trailingChar = false;
		for (GradedBigraph g : iterable) {
			sb.append(g.toString());
			sb.append(",");
			trailingChar = true;
		}
		if (trailingChar) {
			sb.deleteCharAt(sb.length() - 1);
			trailingChar = false;
		}
		sb.append("}");
		return sb.toString();
	}

	public static Iterable<String[]> iterableToStringArrayIterable(
			final Iterable<GradedBigraph> iterable, final int chunksize) {
		return new Iterable<String[]>() {

			public Iterator<String[]> iterator() {
				return new Iterator<String[]>() {

					Iterator<GradedBigraph> iterator = iterable.iterator();

					public boolean hasNext() {
						return iterator.hasNext();
					}

					public String[] next() {
						String[] array = new String[chunksize];
						int i = 0;
						while (i < chunksize && iterator.hasNext()) {
							array[i] = iterator.next().toString();
							++i;
						}
						if (i < chunksize) {
							String[] shortArray = new String[i];
							System.arraycopy(array, 0, shortArray, 0, i);
							return shortArray;
						} else {
							return array;
						}
					}

					public void remove() {
						throw new UnsupportedOperationException();
					}

				};
			}

		};
	}

	public static String[] iterableToStringArray(
			Iterable<GradedBigraph> iterable) {
		List<String> list = new LinkedList<String>();
		for (GradedBigraph g : iterable) {
			list.add(g.toString());
		}
		return list.toArray(new String[list.size()]);
	}

	public static String[][] listOfIterablesToStringArray(
			List<Iterable<GradedBigraph>> iterables) {
		List<String[]> list = new LinkedList<String[]>();
		for (Iterable<GradedBigraph> iterable : iterables) {
			list.add(iterableToStringArray(iterable));
		}
		return list.toArray(new String[list.size()][]);
	}

	private static int[][] multiplyMatrices(List<int[][]> m) {
		return multiplyMatrices(m.toArray(new int[m.size()][][]));
	}

	private static int[][] multiplyMatrices(int[][]... m) {
		assert m.length > 0;
		if (m.length == 1)
			return m[0];
		int[][] m1 = m[0], m2 = m[1];
		assert m1.length > 0;
		assert m2.length > 0;
		assert m2.length == m1[0].length;
		int[][] result = new int[m1.length][m2[0].length];
		for (int i = 0; i < m1.length; ++i) {
			for (int j = 0; j < m2[0].length; ++j) {
				result[i][j] = 0;
				for (int k = 0; k < m2.length; ++k) {
					result[i][j] += m1[i][k] * m2[k][j];
				}
			}
		}
		if (m.length == 2) {
			return result;
		} else {
			int[][][] m_ = new int[m.length - 1][][];
			m_[0] = result;
			for (int l = 2; l < m.length; ++l) {
				m_[l - 1] = m[l];
			}
			return multiplyMatrices(m_);
		}
	}

	private static int[] multiply(int[][] m, int[] v) {
		assert m.length > 0;
		assert m[0].length == v.length;
		int[] result = new int[m.length];
		for (int i = 0; i < m.length; ++i) {
			for (int j = 0; j < m[i].length; ++j) {
				result[i] += m[i][j] * v[j];
			}
		}
		return result;
	}

	private static int[][] transpose(int[][] m) {
		int[][] result = new int[m[0].length][m.length];
		for (int i = 0; i < m.length; ++i) {
			for (int j = 0; j < m[0].length; ++j) {
				result[j][i] = m[i][j];
			}
		}
		return result;
	}

	private int[] monotonePathsToPenultimateDepth() {
		if (monotonePathsToPenultimateDepth == null) {
			List<int[][]> reversedBlocks = new ArrayList<int[][]>(
					adjacencyMatrix.subDiagonalBlocks.subList(0,
							adjacencyMatrix.subDiagonalBlocks.size() - 1));
			Collections.reverse(reversedBlocks);
			monotonePathsToPenultimateDepth = transpose(multiplyMatrices(reversedBlocks))[0];
		}
		return monotonePathsToPenultimateDepth;
	}

	private int[] monotonePathsToUltimateDepth() {
		return multiply(adjacencyMatrix.lastBlock,
				monotonePathsToPenultimateDepth());
	}

	private int sumOfSquares(int[] v) {
		int result = 0;
		for (int p : v) {
			result += (p * p);
		}
		return result;
	}

	private int maximalLoops() {
		return sumOfSquares(monotonePathsToUltimateDepth());
	}

	// this can be done much more efficiently
	private int[] stableLoopCounts() {
		if (stableLoopCounts == null) {

			stableLoopCounts = new int[depth() + 1];

			stableLoopCounts[0] = 1;

			int[][] A = adjacencyMatrix.entireMatrix();
			int[][] AS = multiplyMatrices(A, A);
			int[][] AP = AS;

			for (int i = 1; i < depth(); ++i) {
				stableLoopCounts[i] = AP[0][0];
				AP = multiplyMatrices(AP, AS);
			}

			stableLoopCounts[depth()] = AP[0][0];
		}

		return stableLoopCounts;
	}

//	@SuppressWarnings("unused")
	private int lastStableLoopCount() {
		return stableLoopCounts()[depth()];
	}

	// private static long binomialCoefficient(int n, int k) {
	// long t = 1;
	//        
	// int m = n - k; // r = Math.max(r, n - r);
	// if (k < m) {
	// k = m;
	// }
	//        
	// for (int i = n, j = 1; i > k; i--, j++) {
	// t = t * i / j;
	// }
	//        
	// return t;
	// }
	//
	// private int dimensionOfLastStableLowWeightSpace() {
	// int result = 0;
	// int[] stableLoopCounts = stableLoopCounts();
	// int r = depth();
	// for(int n = 0; n <= depth(); ++n) {
	// result += (-1)^(r - n) * (2 * r * binomialCoefficient(r + n, r - n) / (r
	// + n)) * stableLoopCounts[n];
	// }
	// return result;
	// }

	private int rank() {
		int rank = 0;
		for (int[][] matrix : adjacencyMatrix.subDiagonalBlocks) {
			rank += matrix[0].length;
		}
		rank += adjacencyMatrix.lastBlock.length;
		return rank;
	}

	int rankAtMaximalDepth() {
		return adjacencyMatrix.lastBlock.length;
	}

	int depth() {
		return adjacencyMatrix.subDiagonalBlocks.size();
	}

	public Iterable<GradedBigraph> findExtensionsUpToRankAndDepth(
			final double d, final int totalRank, final int totalDepth) {
		return new TreeIterable<GradedBigraph>(this, totalDepth - depth()) {
			@Override
			public Iterable<GradedBigraph> buildBranches(GradedBigraph g) {
				if (totalRank < 0) {
					return g.findExtensionsUpToRank(d, -1);

				} else if (totalRank > g.rank()) {
					return g.findExtensionsUpToRank(d, totalRank - g.rank());
				} else {
					return Iterables.emptyIterable();
				}
			}
		};
	}

	public static Iterable<GradedBigraph> findExtensionsUpToRankAndDepth(
			Iterable<GradedBigraph> graphs, final double d,
			final int totalRank, final int totalDepth) {
		return new IterableBundle<GradedBigraph, GradedBigraph>(graphs) {

			@Override
			protected Iterable<GradedBigraph> buildNewFibreIterable(
					GradedBigraph g) {
				return g.findExtensionsUpToRankAndDepth(d, totalRank,
						totalDepth);
			}

		};
	}

	public Iterable<GradedBigraph> findExtensions(double d) {
		return findExtensionsUpToRank(d, -1, -1);
	}
	
	public Iterable<GradedBigraph> findExtensionsUpToRank(double d,
			final int additionalRank) {
		return findExtensionsUpToRank(d, additionalRank, -1);
	}

	public Iterable<GradedBigraph> findExtensionsUpToRank(final double d,
			final int additionalRank, final int maximalLoops) {
		GradedBigraph g0 = new GradedBigraph(this, new int[0][]);

		return Iterables.rest(new TreeIterable<GradedBigraph>(g0,
				additionalRank < 0 ? Integer.MAX_VALUE : additionalRank) {

			@Override
			public Iterable<GradedBigraph> buildBranches(GradedBigraph g1) {
				return Iterables.filter(Iterables.filter(g1
						.increaseMaximalRank(d, maximalLoops), connected),
						rowsOrdered);
			}

		});
	}

	public Iterable<GradedBigraph> findExtensionsMatching(final double norm,
			final GradedBigraph graph) {
		return findExtensionsMatching(norm, new GradedBigraph[] { graph }).get(
				0);
	}

	public List<Iterable<GradedBigraph>> findExtensionsMatching(
			final double norm, final GradedBigraph... graphs) {
		int maximalRank = 0;

		for (GradedBigraph graph : graphs) {
			assert graph.depth() == depth() + 1;
			if (graph.rankAtMaximalDepth() > maximalRank) {
				maximalRank = graph.rankAtMaximalDepth();
			}
		}

		List<Iterable<GradedBigraph>> results = new ArrayList<Iterable<GradedBigraph>>(
				graphs.length);

		if (depth() % 2 == 0) {
			// We're adding odd vertices.

			// It's important that we don't restrict the ordering of vertices,
			// because the odd vertices are being implicitly identified.

			GradedBigraph g0 = new GradedBigraph(this, new int[0][]);

			List<GradedBigraph> extensions = Iterables.asLazyList(Iterables
					.rest(new TreeIterable<GradedBigraph>(g0, maximalRank) {

						@Override
						public Iterable<GradedBigraph> buildBranches(
								GradedBigraph g1) {
							return Iterables.filter(g1.increaseMaximalRank(
									norm, -1), connected);
						}

					}));

			for (final GradedBigraph graph : graphs) {
				results.add(Iterables.filter(extensions,
						new Predicate<GradedBigraph>() {
							public Boolean evaluate(GradedBigraph g1) {
								return g1.rankAtMaximalDepth() == graph
										.rankAtMaximalDepth()
										/*
										 * && g1.maximalLoops() ==
										 * graph.maximalLoops()
										 */

										&& g1.lastStableLoopCount() == graph
												.lastStableLoopCount();
							}
						}));
			}
			return results;
		} else {
			// We're adding even vertices

			GradedBigraph g0 = new GradedBigraph(this, new int[0][]);

			List<GradedBigraph> extensions = Iterables
					.asLazyList(Iterables.rest(new TreeIterable<GradedBigraph>(
							g0, Integer.MAX_VALUE) {

						@Override
						public Iterable<GradedBigraph> buildBranches(
								GradedBigraph g1) {
							return Iterables.filter(Iterables.filter(g1
									.increaseMaximalRank(norm, -1), connected),
									rowsOrdered);
						}

					}));

			for (final GradedBigraph graph : graphs) {
				results.add(Iterables.filter(extensions,
						new Predicate<GradedBigraph>() {
							public Boolean evaluate(GradedBigraph g1) {
								return /* true *//*
												 * g1.maximalLoops() ==
												 * graph.maximalLoops()
												 */
								g1.lastStableLoopCount() == graph
										.lastStableLoopCount();
							}
						}));
			}
			return results;

			// return Collections.nCopies(graphs.length,
			// (Iterable<GradedBigraph>) extensions);

		}
	}

	private static final Predicate<GradedBigraph> connected = new Predicate<GradedBigraph>() {

		public Boolean evaluate(GradedBigraph g) {
			for (int[] row : g.adjacencyMatrix.lastBlock) {
				boolean zero = true;
				for (int entry : row) {
					if (entry != 0) {
						zero = false;
						break;
					}
				}
				if (zero)
					return false;
			}
			return true;
		}

	};

	private static final Comparator<int[]> rowComparator = new Comparator<int[]>() {

		public int compare(int[] o1, int[] o2) {
			int result;
			result = o1.length - o2.length;
			if (result != 0)
				return result;
			for (int i = 0; i < o1.length; ++i) {
				result = o1[i] - o2[i];
				if (result != 0)
					return result;
			}
			return 0;
		}

	};

	private static final Predicate<GradedBigraph> rowsOrdered = new Predicate<GradedBigraph>() {

		public Boolean evaluate(GradedBigraph g) {
			int[][] array = g.adjacencyMatrix.lastBlock;
			for (int i = 0; i < array.length - 1; ++i) {
				if (rowComparator.compare(array[i], array[i + 1]) < 0) {
					return false;
				}
			}
			return true;
		}

	};

	private Iterable<GradedBigraph> increaseMaximalRank(final double d,
			final int maximalLoops) {
		return new Iterable<GradedBigraph>() {

			public Iterator<GradedBigraph> iterator() {
				return new Iterator<GradedBigraph>() {
					int[][] matrix = new int[adjacencyMatrix.lastBlock.length + 1][adjacencyMatrix.subDiagonalBlocks
							.get(adjacencyMatrix.subDiagonalBlocks.size() - 2).length];
					{
						for (int i = 0; i < adjacencyMatrix.lastBlock.length; ++i) {
							matrix[i] = adjacencyMatrix.lastBlock[i];
						}
						// matrix[adjacencyMatrix.lastBlock.length][0] = 1;
					}
					int[][][] matrices = new int[adjacencyMatrix.subDiagonalBlocks
							.size()][][];
					{
						for (int i = 0; i < adjacencyMatrix.subDiagonalBlocks
								.size() - 1; ++i) {
							matrices[i] = adjacencyMatrix.subDiagonalBlocks
									.get(i);
						}
						matrices[adjacencyMatrix.subDiagonalBlocks.size() - 1] = matrix;
					}
					GradedBigraph next = new GradedBigraph(matrices);

					public boolean hasNext() {
						return next != null;
					}

					private boolean over() {
						return (next.adjacencyMatrix
								.lowerBoundForGreatestEigenvalue() > d)
								|| (maximalLoops > -1 && next.maximalLoops() > maximalLoops);
					}

					public GradedBigraph next() {
						GradedBigraph result = new GradedBigraph(next);
//						System.out.println(new GradedBigraph(next).toString());
//						System.out.println(next.adjacencyMatrix.lowerBoundForGreatestEigenvalue());
						next.adjacencyMatrix.advanceLastRow();
						while (over()) {
//							System.out.println(new GradedBigraph(next).toString());
//							System.out.println(next.adjacencyMatrix.lowerBoundForGreatestEigenvalue());
							if (!next.adjacencyMatrix.carryLastRow()) {
								next = null;
								break;
							}
						}
						return result;
					}

					public void remove() {
						throw new UnsupportedOperationException();
					}

				};
			}
		};
	}

	public static void main(String... args) {
		{
			int[][] m1 = new int[][] { { 1 } };
			int[][] m2 = new int[][] { { 1 }, { 1 } };
			int[][] m3 = new int[][] { { 1, 0 }, { 0, 1 } };
			int[][] m4 = new int[][] { { 1, 1 } };
			// GradedBigraph d4 = new GradedBigraph(m1, m2);
			// GradedBigraph d5 = new GradedBigraph(m1, m1, m2);
			GradedBigraph hex = new GradedBigraph(m1, m1, m1, m2, m3, m4);
			// GradedBigraph trivalent122 = new GradedBigraph(m1, m2, m3);

			for (int i : hex.stableLoopCounts()) {
				System.out.print(i);
				System.out.print(" ");
			}
			System.out.println();

			long time = System.currentTimeMillis();

			// for (GradedBigraph g : d5.findExtensionsUpToRank(2.5, 4, 12)) {
			// System.out.println(g);
			// }

			System.out.println(System.currentTimeMillis() - time);
		}
	}

}

package org.fusionatlas.v1.odometer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class SymmetricTridiagonalMatrix extends AbstractMatrix {

	final List<int[][]> subDiagonalBlocks;
	final int[][] lastBlock;
	private int[][] entireMatrix;

	public SymmetricTridiagonalMatrix(int[][]... matrices) {
		super(size(matrices));
		subDiagonalBlocks = cloneList(Arrays.asList(matrices));
		lastBlock = subDiagonalBlocks.get(subDiagonalBlocks.size() - 1);
	}

	private static int size(int[][]... matrices) {
		assert matrices.length > 0;
		assert matrices[0].length > 0;
		int size = matrices[0][0].length;
		for (int[][] matrix : matrices) {
			size += matrix.length;
		}
		return size;
	}

	public SymmetricTridiagonalMatrix(SymmetricTridiagonalMatrix matrix) {
		super(matrix.size);
		subDiagonalBlocks = new ArrayList<int[][]>(matrix.subDiagonalBlocks
				.size());
		for (int i = 0; i < matrix.subDiagonalBlocks.size() - 1; ++i) {
			subDiagonalBlocks.add(matrix.subDiagonalBlocks.get(i));
		}
		subDiagonalBlocks.add(cloneArray(matrix.subDiagonalBlocks
				.get(matrix.subDiagonalBlocks.size() - 1)));
		lastBlock = subDiagonalBlocks.get(subDiagonalBlocks.size() - 1);
		approximateFPEigenvector = matrix.approximateFPEigenvector;
	}

	public SymmetricTridiagonalMatrix(SymmetricTridiagonalMatrix previous,
			int[][] matrix) {
		super(previous.size + matrix.length);
		subDiagonalBlocks = new ArrayList<int[][]>(previous.subDiagonalBlocks
				.size() + 1);
		subDiagonalBlocks.addAll(previous.subDiagonalBlocks);
		subDiagonalBlocks.add(cloneArray(matrix));
		lastBlock = subDiagonalBlocks.get(subDiagonalBlocks.size() - 1);
		if (previous.approximateFPEigenvector != null) {
			approximateFPEigenvector = new float[size];
			System.arraycopy(previous.approximateFPEigenvector, 0,
					approximateFPEigenvector, 0, previous.size);
			for (int i = previous.size; i < size; ++i) {
				approximateFPEigenvector[i] = 1;
			}
		}
	}

	private int[][] cloneArray(int[][] array) {
		int[][] result = new int[array.length][];
		for (int i = 0; i < array.length; ++i) {
			result[i] = new int[array[i].length];
			System.arraycopy(array[i], 0, result[i], 0, array[i].length);
		}
		return result;
	}

	private List<int[][]> cloneList(List<int[][]> list) {
		List<int[][]> result = new ArrayList<int[][]>(list.size());
		for (int[][] array : list) {
			result.add(cloneArray(array));
		}
		return result;
	}

	public float[] multiply(float[] vector) {
		float[] result = new float[size];
		for(int i = 0; i < size; ++i) {
			result[i] = 0;
		}
		
		int rowOffset, columnOffset;

		// first the sub-diagonal blocks
		rowOffset = subDiagonalBlocks.get(0).length;
		columnOffset = 0;
		for (int[][] block : subDiagonalBlocks) {
			for (int i = 0; i < block.length; ++i) {
				for (int j = 0; j < block[i].length; ++j) {
					result[rowOffset + i] += block[i][j] *
							vector[columnOffset + j];
					assert result[rowOffset + i] >= 0;
				}
			}
			rowOffset += block.length;
			columnOffset += block[0].length;
		}

		// then the super-diagonal blocks (which are just transposes).
		rowOffset = 0;
		columnOffset = subDiagonalBlocks.get(0).length;
		for (int[][] block : subDiagonalBlocks) {
			for (int i = 0; i < block[0].length; ++i) {
				for (int j = 0; j < block.length; ++j) {
					result[rowOffset + i] += block[j][i] * vector[columnOffset + j];
					assert result[rowOffset + i] >= 0;
				}
			}
			rowOffset += block[0].length;
			columnOffset += block.length;
		}

		return result;
	}

	public int[][] entireMatrix() {
		if (entireMatrix == null) {
			entireMatrix = new int[size][size];

			int rowOffset, columnOffset;

			// first the sub-diagonal blocks
			rowOffset = subDiagonalBlocks.get(0).length;
			columnOffset = 0;
			for (int[][] block : subDiagonalBlocks) {
				for (int i = 0; i < block.length; ++i) {
					for (int j = 0; j < block[i].length; ++j) {
						entireMatrix[rowOffset + i][columnOffset + j] = block[i][j];
					}
				}
				if (block.length != 0) {
					rowOffset += block.length;
					columnOffset += block[0].length;
				}
			}

			// then the super-diagonal blocks (which are just transposes).
			rowOffset = 0;
			columnOffset = subDiagonalBlocks.get(0).length;
			for (int[][] block : subDiagonalBlocks) {
				if (block.length != 0) {
					for (int i = 0; i < block[0].length; ++i) {
						for (int j = 0; j < block.length; ++j) {
							entireMatrix[rowOffset + i][columnOffset + j] += block[j][i];
						}
					}
					rowOffset += block[0].length;
					columnOffset += block.length;
				}
			}
		}
		return entireMatrix;
	}

	public void advance() {
		approximateFPEigenvector = null;
		++lastBlock[0][0];
	}

	public void advanceLastRow() {
		approximateFPEigenvector = null;
		++lastBlock[lastBlock.length - 1][0];
	}

	public boolean carry() {
		for (int i = 0; i < lastBlock.length; ++i) {
			for (int j = 0; j < lastBlock[i].length; ++j) {
				if (lastBlock[i][j] != 0) {
					approximateFPEigenvector = null;
					lastBlock[i][j] = 0;
					++j;
					if (j == lastBlock[i].length) {
						j = 0;
						++i;
					}
					if (i == lastBlock.length) {
						return false;
					} else {
						++lastBlock[i][j];
						return true;
					}
				}
			}
		}
		// all matrix entries were zero.
		assert false;
		return false;
	}

	public boolean carryLastRow() {
		int i = lastBlock.length - 1;
		for (int j = 0; j < lastBlock[i].length; ++j) {
			if (lastBlock[i][j] != 0) {
				approximateFPEigenvector = null;
				lastBlock[i][j] = 0;
				++j;
				if (j == lastBlock[i].length) {
					j = 0;
					++i;
				}
				if (i == lastBlock.length) {
					return false;
				} else {
					++lastBlock[i][j];
					return true;
				}
			}
		}
		// all matrix entries were zero.
		assert false;
		return false;
	}

	public static void main(String[] args) {
		{
			int[][] m1 = new int[][] { { 1, 1 }, { 1, 1 } };
			int[][] m2 = new int[][] { { 1, 0 }, { 1, 0 } };

			Matrix m = new SymmetricTridiagonalMatrix(m1, m2);
			System.out.println(m.lowerBoundForGreatestEigenvalue());
			System.out.println(m.lowerBoundForGreatestEigenvalue());

		}
		{
			int[][] m1 = new int[][] { { 1 } };
			int[][] m2 = new int[][] { { 1 }, { 1 } };
			int[][] m3 = new int[][] { { 1, 0 }, { 0, 1 } };

			Matrix haagerup = new SymmetricTridiagonalMatrix(m1, m1, m1, m2,
					m3, m3);
			System.out.println(haagerup.lowerBoundForGreatestEigenvalue());
		}
	}

}

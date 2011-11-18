import x10.util.Random;

public class Knapsack {
	public var debug:Boolean = false; 
	
	public var maxW:Int = 0;  // the weight limit
	public var size:Int = 0;  // the size of the items
	public var numAsyncs:Int = 0; // the number of asyncs
	public var blockSize:Int = 0; // data read block size
	public var numBlock:Int = 0;  // data read block count 
	/* the weight of each item */
	public var weight:Array[Int] = null;
	/* the value of each item */
	public var value:Array[Int] = null;
	
	/* for profiling purpose */
	public var serialTime:Long=0;
	public var parallelTime:Long=0;
	private static val Meg = 1000*1000;
	
	/* Constructor */
	public def this(item_size:Int, max_weight:Int, numAsyncs:Int, debug:Boolean) {
		size = item_size;
		maxW = max_weight;
		// numAsync number should not be greater than the 
		// item number
		if (numAsyncs > item_size)
			this.numAsyncs = item_size + 1;  // add for the 0th row
		else
		    this.numAsyncs = numAsyncs;
		this.debug = debug;
		blockSize = max(1, (maxW + 1) / numAsyncs);
		numBlock = (maxW + 1) / blockSize + (((maxW + 1) % blockSize == 0) ? 0 : 1);
	}
	
	/*
	 * Sequential version of 0-1 knapsack using Dynamic Programming
	 * for the details: http://en.wikipedia.org/wiki/Knapsack_problem
	 * (Please search for 0-1 'knapsack' in the page. But reading from the
	 * top helps you better understand the problem if you are not familiar
	 * with 'Knapsack problem'
	 * 
	 * Return value:
	 * The max value we can pick with total weight <= maxW
	 */
	public def knapsackSeq():Int {
		/*
		 * If you are confused by the following statements, 
		 * please come back here after you skim the code
		 * 
		 * Define m[i, w] to be the maximum value that 
		 * can be attained with weight less than or equal to w 
		 * using items up to i. 
		 * 
		 * O(nw) space is unnecessary. Use a buffer
		 * m[w] = m[i,w]
		 * buf[w] = m[i-1, w]
		 * O(2*w) space is enough.
		 */
		
		val time = System.nanoTime();
		
		/* m[w] is expected to store the maximum value that can be attained with  
		 * total weight less than or equal to w. m[maxW] then is the solution to
		 * the problem. */
		var m:Array[Int] = new Array[Int](maxW + 1);
		/* The memory buffer for updating m[] */
		var buf:Array[Int] = new Array[Int](maxW + 1);
		
		/* fill m[w] and buf[w] with 0, i.e. m[0, w] = 0 for all w */
		for (w in (0..maxW)) {
			m(w) = 0;
			buf(w) = 0;
		}
		
		/*
		 * The induction rules for dynamic programming
		 * (Althiough this implementation use one dimensional array (m[w]) to 
		 * represent two dimensional array (m[i,w]) as mentioned above, we use
		 * two dimensional array here for easy explanation.
		 * 
		 * rule 1. m[0, w]=0
		 * rule 2. m[i, 0]=0
		 * rule 3. m[i, w]=m[i-1,w] if wi > w (the new item is more 
		 *         than the current weight limit)
		 * rule 4. m[i, w]=max(m[i-1,w], m[i-1,w-wi]+vi) if wi <= w.
		 * /

		/* 
		 * i stands for the max number of items we can pick to obtain the
		 * max value
		 * start from i = 1, and m[maxW] when i == weight.size would be the
		 * final max value
		 */
		for (i in (1..weight.size)) {
			/* swap m and buf, then buf contains the max values we get in 
			 * the last iteration. */
			val tmp:Array[Int] = buf;
			buf = m;
			m = tmp;
			
			/* idx for weight and value */
			val idx:Int = i - 1;
			
			/* Calcuate each m[w] for w < maxW when we can only pick from the
			 * first i items. */
			for (w in (1..maxW)) {
				/* Use rule 3 and 4 described above */
				if (weight(idx) > w) {
					m(w) = buf(w);
				} else {
					m(w) = max(buf(w), buf(w - weight(idx)) + value(idx));
				}
			}
			
			/* print debug information */
			if (debug) {
				for (pi in (0.. (m.size - 1)))
					Console.OUT.print(m(pi) + ",");
				Console.OUT.println();
			}
		}
		serialTime += (System.nanoTime()-time)/Meg;
		return m(maxW); 
	}
	
	/* Write a single block of data in a row in the DP table */
	private def writeSingleBlock(row:Int, blockIdx:Int, m:Array[Int](2)) {
		val arrIdx:Int = row % numAsyncs;
		val prevIdx:Int = (arrIdx - 1 + numAsyncs) % numAsyncs;
		val weightIdx:Int = row - 1;
		val start:Int = blockIdx * blockSize;
		val end:Int = blockIdx == numBlock - 1 ? maxW : start + blockSize - 1;
		
		for (w in (start..end)) {
			/* Use rule 2, 3 and 4 described above */
			if (w == 0) {
				continue;
			} else if (weight(weightIdx) > w) {
				m(arrIdx, w) = m(prevIdx, w);
			} else {
				m(arrIdx, w) = max(m(prevIdx, w), 
						m(prevIdx, w - weight(weightIdx)) + value(weightIdx));
			}
		}
	}
	
	/* Write a single row in the DP table */
	private def writeSingleRow(row:Int, m:Array[Int](2), blockFinish:Array[Int](1)) {
		val arrIdx:Int = row % numAsyncs;
		val prevIdx:Int = (arrIdx - 1 + numAsyncs) % numAsyncs;
		
		/* Wait until the memory of this row is not used any more */
		while(blockFinish(arrIdx) != -1);
		
		/* for each block in the row */
		for (bi in (0..(numBlock - 1))) {
			/* Wait until the name block of the previous row has been filled */
			while (blockFinish(prevIdx) < bi);
			
		    /* Calculate the values in the signle block */
		    writeSingleBlock(row, bi, m);
		    
		    /* Update blockFinish for this row */
			blockFinish(arrIdx) = bi;
		}
				
		/* update prev thread's buffer state to -1 to mark that the memory space of
		 * this row can be reused now */
		blockFinish(prevIdx) = -1;
	}
	
	/* the Parallel version's main entry */
	public def knapsackPar():Int {
		val time = System.nanoTime();
		var result:Int = 0;
		/* m[i, w] is expected to store the maximum value that can be attained with  
		 * total weight less than or equal to w using the first 'i' items. m[size, maxW] 
		 * then is the solution to the problem. */
		
		/* 
		 * The Dynamic Programming (DP) Table 
		 *                       w
		 *         -----------------------------     |
		 *         | 0 | 0 | 0 | 0 | 0 | 0 | 0 |     |
		 *         -----------------------------    \|/
		 *         | 0 |   |   |   |   |   |   |
		 *         -----------------------------
		 *         | 0 | b | c | d |   |   |   |
		 *      i  -----------------------------
		 *         | 0 |   |   | a |   |   |   |
		 *         -----------------------------
		 *         | 0 |   |   |   |   |   |   |
		 *         -----------------------------
		 *         | 0 |   |   |   |   |   |   |
		 *         -----------------------------
         *         | 0 |   |   |   |   |   | X |
		 *         -----------------------------
		 * 
		 *         X is what we want to find. (m[item_size, maxW))
		 *         For each element in the DP table, according to the following rules, its 
		 *         value depends on the elments in the row above it with smaller 'w'. 
		 *         For example, in the above fig, a's value depends on b, c, and d.
		 * 
		 *         *** This dependency is the main contraints of the calculation order of our
		 *         *** parallism, which is common to a large class of DP problems.
		 *         
		 *         Evolving rules:
		 * 		   rule 1. m[0, w]=0
		 *         rule 2. m[i, 0]=0
	     *         rule 3. m[i, w]=m[i-1,w] if wi > w (the new item is more 
         *                 than the current weight limit)
         *         rule 4. m[i, w]=max(m[i-1,w], m[i-1,w-wi]+vi) if wi <= w.
		 */
		
		/* The DP table, instead of creating an 'item_size x (maxW+1)' size table, 
		 * we create an 'numAsyncs x (maxW+1)' table for saving memory space. 
		 * We can do so because the values in a row only depend on the one row right
		 * above it. Once the Nth row is calculated, the rows before (N-1)th row become
		 * useless. Therefore we can reuse them.
		 */
		var m:Array[Int](2) = new Array[Int]((0..(numAsyncs - 1))*(0..maxW), 0);
		
		/* Used to indicate how many blocks have finished calculating in a row. 
		 * 
		 * We divide every row in the DP table into a bunch of 'blocks'. A 'block' is
		 * the minimum unit for dependency checking. We introduce 'block' because we 
		 * don't want to do dependency check when calculating every element in the table, 
		 * which will slow down the execution. 
		 * 
		 * The following array 'blockFinish' is used to keep track of the current block 
		 * being calculated in each row, which tells how many blocks have finished.
		 * 
		 * The condition for a block to meet the dependency to be calculated is as simple as:
		 * For the jth block in ith row Bij, if ony B(i-1)j is calculated, then we can make
		 * use every element Bij depends on have been calculated. (If we calculate from left
		 * to right for every row.)
		 * 
		 * fill bufferFinish(t) with i (0 based) when thread t finishes writing block i
		 * initialize bufferState(0..(numAsyncs-1)) with -1
		 * initialize bufferState(0) with (numBuffer - 1) 
		 */
		var blockFinish:Array[Int] = new Array[Int](numAsyncs, -1);

		/* The first row is already initialized with all 0's. So mark it as finished */
		blockFinish(0) = numBlock - 1;
		
		/* Each async activity calculates one row in the (i x w) DP cache array, and then
		 * move to another row 'numAsyncs' away 
		 */
		finish for (t in 0..(numAsyncs - 1)) {
			async {
				var row:Int = t;
				
				while (row <= size) {
					/* Calculate dynamical program values for the current row */
					/* Skip row 0 since it's initialized to all zero already */
				    if (row > 0) writeSingleRow(row, m, blockFinish);
					
					/* If the current row is the last row, store the result */
					if (row == size) result = m(t, maxW);
					
					/* continue to calculate the row "numAsync" rows below */
					row += numAsyncs;
				}   
			}  // end of async
		}  // end of finish
		
		parallelTime += (System.nanoTime()-time)/Meg;
		return result;
	}
	
	public static def max(x:Int, y:Int):Int {
		if (x > y)
			return x;
		else
			return y;
	}
	
	/*
	 * Randomly generate the weights and values
	 */
	public def generateRandomData() {
		weight = new Array[Int](size);
		value = new Array[Int](size);
		val ran = new Random();
		
		for (i in 0..(size-1)) {
			weight(i) = ran.nextInt(maxW) + 1;
			if (ran.nextInt(4) == 0) weight(i) /= 2; 
			value(i) = ran.nextInt(100) + 1;
		}
	}
	
	/*
	 * Print the basic information of the data of this Knapsack
	 * problem
	 */
	public def print() {
		Console.OUT.println("size:  " + size);
		Console.OUT.println("weight limit:  " + maxW);
		Console.OUT.println("item weights: ");
		for (i in 0..(size-1)) {
			Console.OUT.print(weight(i) + " ");
		}
		Console.OUT.println("");
		Console.OUT.println("item values: ");
		for (i in 0..(size-1)) {
			Console.OUT.print(value(i) + " ");
		}
		Console.OUT.println("");
	}
	
	// Simple version of print
	public static def print(s:String) {
		Console.OUT.println(s);
	}
	
	public static def main(args:Array[String]) {
		/* input check */
		if (args.size < 3) {
			Console.OUT.println(
					"Usage: Knapsack <size> <max_weight> <func_test>");
			return;
		}
		
		val size = Int.parseInt(args(0));
		val maxW = Int.parseInt(args(1));
		val numAsyncs = Int.parseInt(args(2));
		val debug = Boolean.parseBoolean(args(3));
		
		var ks:Knapsack = new Knapsack(size, maxW, numAsyncs, debug);
		ks.generateRandomData();
		if (debug) ks.print();
		
		val seqResult:Int = ks.knapsackSeq();
		val parResult:Int = ks.knapsackPar();
		Console.OUT.println("The max value we can get with weight limit '" 
				+ maxW + "' is (Sequential version): " + seqResult);
		Console.OUT.println("The max value we can get with weight limit '" 
				+ maxW + "' is (Parallel version): " + parResult);
		Console.OUT.println();
		if (seqResult == parResult) {
			Console.OUT.println("[Done.] Time to compute serially is " + ks.serialTime
					+ ", and to compute in parallel is " + ks.parallelTime);
		} else {
			Console.OUT.println("[Error.] Inconsistent result!");
		}
	}
}

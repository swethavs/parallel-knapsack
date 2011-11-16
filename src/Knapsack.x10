import x10.util.Random;

public class Knapsack {
	private static val debug:Boolean = false; 
	
	public var maxW:Int = 0;  // the weight limit
	public var size:Int = 0;  // the size of the items
	/* the weight of each item */
	public var weight:Array[Int] = null;
	/* the value of each item */
	public var value:Array[Int] = null;
	
	/* for profiling purpose */
	public var serialTime:Long=0;
	public var parallelTime:Long=0;
	private static val Meg = 1000*1000;
	
	/* Constructor */
	public def this(item_size:Int, max_weight:Int) {
		size = item_size;
		maxW = max_weight;
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
		 * Please you are confused by the following statements, come back
		 * here after you skim the code
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
	
	/* the Parallel version to be implemented */
	public def knapsackPar():Int {
		return 0;
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
	
	public static def main(args:Array[String]) {
		/* input check */
		if (args.size < 3) {
			Console.OUT.println(
					"Usage: Knapsack <size> <max_weight> <func_test>");
			return;
		}
		
		val size = Int.parseInt(args(0));
		val maxW = Int.parseInt(args(1));
		val funcTest = Boolean.parseBoolean(args(2));
		
		var ks:Knapsack = new Knapsack(size, maxW);
		ks.generateRandomData();
		if (funcTest) ks.print();
		
		Console.OUT.println("The max value we can get with weight limit '" 
				+ maxW + "' is (Sequential version): " + ks.knapsackSeq());
		Console.OUT.println("The max value we can get with weight limit '" 
				+ maxW + "' is (Parallel version): " + ks.knapsackPar());
		Console.OUT.println("");
		Console.OUT.println("[Done.] Time to compute serially is " + ks.serialTime
				+ ", and to compute in parallel is " + ks.parallelTime);
	}
}
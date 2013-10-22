package util.random;

import java.math.BigInteger;

public abstract class Random {

	protected BigInteger _seed;
	
	public Random() {
		 this(System.currentTimeMillis());
	}
	
	public Random(long seed) {
		this._seed = BigInteger.valueOf(seed);
	}
	
	public long getSeed() {
		return _seed.longValue();
	}
	
	public void setSeed(long seed) {
		this._seed = BigInteger.valueOf(seed);
	}
	
	public abstract int next(int n);
	public abstract int nextInt();
	public abstract int nextInt(int n);
}

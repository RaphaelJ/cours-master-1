package util.random;

import java.math.BigInteger;

public class LCGRandom extends Random {

	private final BigInteger A = BigInteger.valueOf(25214903917L);
	private final BigInteger C = BigInteger.valueOf(11);
	private final BigInteger M = BigInteger.ONE.shiftLeft(48);

	public LCGRandom() {
		super();
	}
	
	public LCGRandom(long seed) {
		super(seed);
	}
	
	@Override
	public int next(int n) {
		this._seed = this._seed.multiply(this.A).add(this.C).mod(this.M);
		return this._seed.shiftRight(n).intValue();
	}
	
	@Override
	public int nextInt() {
		return next(32);
	}
	
	@Override
	public int nextInt(int n) {
		return next(32) % n;
	}
}

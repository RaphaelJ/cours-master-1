package util.random;

/** Implements a linear congruential generator (LGC) as described in 
 * http://docs.oracle.com/javase/6/docs/api/java/util/Random.html#next(int) */
public class LCGRandom extends Random {
    public LCGRandom()
    {
        super();
    }

    public LCGRandom(long seed)
    {
        super(seed);
    }

    @Override
    public int next(int bits)
    {
        // This linear congruential pseudorandom number generator generates
        // a pseudorandom number of 48 bits. The values are those used by the
        // Java's standard library.
        this._seed = (this._seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1);

        // Returns the most signifiant bits of the seed.
        return Math.abs((int) (this._seed >>> (48 - bits)));
    }
}

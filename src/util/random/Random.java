package util.random;

/** Provides a superclass for pseudorandom number generators.
 * Subclasses must implement the next(int) method. This class is very similar to
 * java.util.Random. */
public abstract class Random {
    protected long _seed;

    public Random()
    {
        this(System.currentTimeMillis());
    }

    public Random(long seed)
    {
        this._seed = seed;
    }

    /** This is the method subclasses need to implement to generate a
     * pseudorandom number of the given number of bits. */
    protected abstract int next(int bits);

    public long getSeed()
    {
        return _seed;
    }

    public void setSeed(long seed)
    {
        this._seed = seed;
    }

    public int nextInt()
    {
        return next(32);
    }

    /** Returns a pseudorandom int value between 0 (inclusive) and the
     * specified value (exclusive). */
    public int nextInt(int n)
    {
        return next(32) % n;
    }
}

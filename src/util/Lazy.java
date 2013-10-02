package util;

import java.util.concurrent.Callable;

/** This object lazily evaluates its value only when first accessed, by
 * executing the function which has been given to the constructor. */
public class Lazy<T> {
    private final Callable<T> _f;
    private boolean _evaluated = false;
    private T _value;

    public Lazy(Callable<T> f)
    {
        this._f = f;
    }

    public T get() throws Exception
    {
        if (!this._evaluated)
            this._value = this._f.call();

        return this._value;
    }
}

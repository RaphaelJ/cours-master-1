package model;

import java.io.Serializable;

public class Coordinates implements Serializable {

	private static final long serialVersionUID = -2199910363902540248L;

	private final int _x;
    private final int _y;

    public Coordinates(int x, int y)
    {
        this._x = x;
        this._y = y;
    }

    public int getX()
    {
        return this._x;
    }

    public int getY()
    {
        return this._y;
    }

    @Override
    public String toString()
    {
        return String.format("%d - %d", this._x, this._y);
    }
}

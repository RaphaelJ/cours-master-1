package model;

public class Coordinates implements Serializable {
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

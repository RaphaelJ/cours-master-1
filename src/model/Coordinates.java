package model;

public class Coordinates {
    private final int _x;
    private final int _y;

    public Coordinates(int x, int y)
    {
        this._x = x;
        this._y = y;
    }

    public int getX()
    {
        return x;
    }

    public int getY()
    {
        return y;
    }

    @Override
    public String toString()
    {
        return String.format("%d - %d", x, y);
    }
}

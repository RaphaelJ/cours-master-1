package model.config;

public abstract class Config {

    protected int _boardWidth;
    protected int _boardHeight;

    protected boolean _useImages;

    public Config()
    {
        this._boardWidth = 10;
        this._boardHeight = 22;
        this._useImages = true;
    }

    public int getBoardWidth()
    {
        return this._boardWidth;
    }

    public int getBoardHeight()
    {
        return this._boardHeight;
    }

    public boolean isUseImages()
    {
        return this._useImages;
    }

    public void setBoardWidth(int boardWidth)
    {
        this._boardWidth = boardWidth;
    }

    public void setBoardHeight(int boardHeight)
    {
        this._boardHeight = boardHeight;
    }

    public void setUseImages(boolean useImages)
    {
        this._useImages = useImages;
    }
}

package viewmodel;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.ImageObserver;

public class ColorTexture {

    private Color _color = null;

    public ColorTexture(Color color)
    {
        this._color = color;
        this._useImage = false;
    }

    public void draw(Graphics g, int x, int y, ImageObserver observer)
                    throws Exception
    {
        int tileSize = PieceViewModel.TILES_SIZE;

        // Fill with color
        g.setColor(this._color);
        g.fillRect(x+1, y+1, tileSize-1, tileSize-1);

        // Draw borders
        g.setColor(Color.BLACK);
        g.drawRect(x, y, tileSize-1, tileSize-1);
    }
}

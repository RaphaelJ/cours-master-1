package view.piece;

import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;

import util.Lazy;

public class ImageTexture implements Texture {

    private BufferedImage _image = null;

    public ImageTexture(BufferedImage image)
    {
        this._image = image;
    }

    public void draw(Graphics g, int x, int y, ImageObserver observer)
                    throws Exception
    {
        g.drawImage(this._image, x, y, observer);
    }
}

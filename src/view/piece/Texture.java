package view.piece;

import java.awt.Graphics;
import java.awt.image.ImageObserver;

public interface Texture {
    public void draw(Graphics g, int x, int y, ImageObserver observer)
                    throws Exception;
}

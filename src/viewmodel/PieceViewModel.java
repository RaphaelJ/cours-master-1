package viewmodel;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;
import java.io.File;
import java.util.concurrent.Callable;

import javax.imageio.ImageIO;

import util.Lazy;

import model.piece.Piece;

public class PieceViewModel {

	/** Size of images which will be used by graphical interfaces to paint
     * cells. */
    public static final int TILES_SIZE = 24;
    public static final File TILES_DIR = new File("resources");
	
	private Piece _piece;
	private Texture _texture;
	
	public PieceViewModel(Piece piece, boolean useImage) {
		this._piece = piece;
		
		switch(piece.getClass().getSimpleName()) {
		
		case "PieceI":
			if(useImage)
				this._texture = new Texture(getTile("cyan.png"));
			else
				this._texture = new Texture(Color.CYAN);
			break;
		case "PieceJ":
			if(useImage)
				this._texture = new Texture(getTile("gray.png"));
			else
				this._texture = new Texture(Color.GRAY);
			break;
		case "PieceL":
			if(useImage)
				this._texture = new Texture(getTile("blue.png"));
			else
				this._texture = new Texture(Color.BLUE);
			break;
		case "PieceO":
			if(useImage)
				this._texture = new Texture(getTile("yellow.png"));
			else
				this._texture = new Texture(Color.YELLOW);
			break;
		case "PieceS":
			if(useImage)
				this._texture = new Texture(getTile("green.png"));
			else
				this._texture = new Texture(Color.GREEN);
			break;
		case "PieceT":
			if(useImage)
				this._texture = new Texture(getTile("purple.png"));
			else
				this._texture = new Texture(Color.MAGENTA);
			break;
		case "PieceZ":
			if(useImage)
				this._texture = new Texture(getTile("red.png"));
			else
				this._texture = new Texture(Color.RED);
			break;
		}
	}
	
	public Piece getPiece() {
		return this._piece;
	}
	
	public void drawTexture(Graphics g, int x, int y, ImageObserver observer)
			throws Exception {
		this._texture.draw(g, x, y, observer);
	}

    /** Returns the tile corresponding to the given name.
     * Doesn't load the image until its first accessed as some interfaces
     * (CLI) don't use them. */
    public static Lazy<BufferedImage> getTile(final String name)
    {
        return new Lazy<BufferedImage>(
            new Callable<BufferedImage>() {
                public BufferedImage call() throws Exception
                {
                    return ImageIO.read(new File(TILES_DIR, name));
                }
            }
        );
    }
}

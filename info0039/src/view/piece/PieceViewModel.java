package view.piece;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;
import java.io.File;
import java.util.concurrent.Callable;

import javax.imageio.ImageIO;

import model.piece.Piece;
import model.piece.PieceBlock;
import model.piece.PieceI;
import model.piece.PieceJ;
import model.piece.PieceL;
import model.piece.PieceO;
import model.piece.PieceS;
import model.piece.PieceT;
import model.piece.PieceZ;
import util.Lazy;

/** Proxy which provides drawing capabilities for each piece. */
public class PieceViewModel {

    /** Size of images which will be used by graphical interfaces to paint
     * cells. */
    public static final int TILES_SIZE = 24;
    public static final File TILES_DIR = new File("resources");

    // Caches tiles in static lazy fields, so images are only loaded once when
    // first needed.
    public static final Lazy<BufferedImage> CYAN   = getTile("cyan.png");
    public static final Lazy<BufferedImage> GRAY   = getTile("gray.png");
    public static final Lazy<BufferedImage> BLUE   = getTile("blue.png");
    public static final Lazy<BufferedImage> YELLOW = getTile("yellow.png");
    public static final Lazy<BufferedImage> GREEN  = getTile("green.png");
    public static final Lazy<BufferedImage> PURPLE = getTile("purple.png");
    public static final Lazy<BufferedImage> RED    = getTile("red.png");

    private Piece _piece;
    private Texture _texture;

    public PieceViewModel(Piece piece, boolean useImage)
    {
        this._piece = piece;

        try {
            if (piece instanceof PieceI || piece instanceof PieceBlock) {
                if (useImage)
                    this._texture = new ImageTexture(CYAN.get());
                else
                    this._texture = new ColorTexture(Color.CYAN);
            } else if (piece instanceof PieceJ) {
                if (useImage)
                    this._texture = new ImageTexture(GRAY.get());
                else
                    this._texture = new ColorTexture(Color.GRAY);
            } else if (piece instanceof PieceL) {
                if (useImage)
                    this._texture = new ImageTexture(BLUE.get());
                else
                    this._texture = new ColorTexture(Color.BLUE);
            } else if (piece instanceof PieceO) {
                if (useImage)
                    this._texture = new ImageTexture(YELLOW.get());
                else
                    this._texture = new ColorTexture(Color.YELLOW);
            } else if (piece instanceof PieceS) {
                if (useImage)
                    this._texture = new ImageTexture(GREEN.get());
                else
                    this._texture = new ColorTexture(Color.GREEN);
            } else if (piece instanceof PieceT) {
                if (useImage)
                    this._texture = new ImageTexture(PURPLE.get());
                else
                    this._texture = new ColorTexture(Color.MAGENTA);
            } else if (piece instanceof PieceZ) {
                if (useImage)
                    this._texture = new ImageTexture(RED.get());
                else
                    this._texture = new ColorTexture(Color.RED);
            } else // Unknown piece
                this._texture = new ColorTexture(Color.BLACK);
        } catch (Exception e) { // Error while loading the image.
            this._texture = new ColorTexture(Color.BLACK);
        }
    }

    public Piece getPiece()
    {
        return this._piece;
    }

    public void drawTexture(Graphics g, int x, int y, ImageObserver observer)
                    throws Exception
    {
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

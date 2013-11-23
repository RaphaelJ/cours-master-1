package viewmodel;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;

import util.Lazy;

public class Texture {

	private Color _color = null;
	private Lazy<BufferedImage> _image = null;
	private boolean _useImage;
	
	public Texture(Lazy<BufferedImage> image) {
		this._image = image;
		this._useImage = true;
	}
	
	public Texture(Color color) {
		this._color = color;
		this._useImage = false;
	}
	
	public void draw(Graphics g, int x, int y, ImageObserver observer)
			throws Exception
	{
		if(this._useImage) {
			g.drawImage(this._image.get(), x, y, observer);
		}
		
		else {
			int tileSize = PieceViewModel.TILES_SIZE;
			
			// Fill with color
			g.setColor(this._color);
			g.fillRect(x+1, y+1, tileSize-1, tileSize-1);
			
			// Draw borders
			g.setColor(Color.BLACK);
			g.drawRect(x, y, tileSize-1, tileSize-1);
		}
	}
}

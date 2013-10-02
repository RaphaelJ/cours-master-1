package model;

import java.awt.image.BufferedImage;

public class Cell {

	private boolean _block;
	private BufferedImage _tile = null;
	
	public Cell(boolean block) {
		_block = block;
	}
	
	public boolean isBlock() {
		return _block;
	}
	
	public void setBlock(boolean block) {
		this._block = block;
	}
	
	public BufferedImage getTile() {
		return _tile;
	}
	
	public void setTile(BufferedImage tile) {
		this._tile = tile;
	}
}

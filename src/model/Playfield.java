package model;

public class Playfield {

	private int width;
	private int height;
	private Cell[][] grid;
	
	public Playfield(int width, int height) {
		
		this.width = width;
		this.height = height;
		
		this.grid = new Cell[height][width];
	}
	
	public int getWidth() {
		return width;
	}
	
	public int getHeight() {
		return height;
	}
}

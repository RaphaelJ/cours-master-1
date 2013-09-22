package model;

public class RotationRectangle {

	private int width;
	private int height;
	private Coordinates center;
	
	public RotationRectangle(int width, int height, Coordinates center) {
		super();
		this.width = width;
		this.height = height;
		this.center = center;
	}
	
	public int getWidth() {
		return width;
	}
	
	public int getHeight() {
		return height;
	}
	
	public Coordinates getCenter() {
		return center;
	}
}

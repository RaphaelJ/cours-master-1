package model.piece;

import java.util.ArrayList;
import java.util.List;

import model.Coordinates;

public abstract class Piece {

	// The rectangle within which the piece is rotated
	protected RotationRectangle rectangle;
	
	// Represents the coordinates list of the piece's blocks inside the rotation rectangle
	protected List<Coordinates> blocks = new ArrayList<Coordinates>();
	
	protected Coordinates position;
	
	public Piece(Coordinates initialPosition) {

		this.position = initialPosition;
	}
	
	// Perform a counter clockwise rotation of the piece
	public void rotate() {
		
		int newX = 0;
		int newY = 0;
		double angle = Math.toRadians(90);
		Coordinates center = rectangle.getCenter();

		for(Coordinates coord : blocks) {
			
			// Translate the coordinates to make the center as origin point (0,0)
			coord.setX(coord.getX() - center.getX());
			coord.setY(coord.getY() - center.getY());

			/* Multiply by -1 the Y coordinate as the origin of the plane (0,0)
			   is at the upper left corner instead of the bottom left one. */
			coord.setY(coord.getY() * -1);
			
			// Rotate the coordinates counter clockwise
			newX = coord.getX() * (int) Math.cos(angle) - coord.getY() * (int) Math.sin(angle);
			newY = coord.getX() * (int) Math.sin(angle) + coord.getY() * (int) Math.cos(angle);
			
			// Reset the Y coordinate and translate back the point
			newY *= -1;
			newX += center.getX();
			newY += center.getY();
			
			// Prevent from the coordinates to be outside the rotation rectangle
			coord.setX((rectangle.getWidth() + newX) % rectangle.getWidth());
			coord.setY((rectangle.getHeight() + newY) % rectangle.getHeight());
		}
	}
	
	public boolean[][] getBlocksPositions() {
		
		boolean[][] positions = new boolean[rectangle.getHeight()][rectangle.getWidth()];
		
		for(boolean[] rows : positions)
			for(boolean col : rows)
				col = false;
		
		for(Coordinates coord : blocks)
			positions[coord.getY()][coord.getX()] = true;
		
		return positions;
	}
	
	public RotationRectangle getRectangle() {
		return rectangle;
	}
}

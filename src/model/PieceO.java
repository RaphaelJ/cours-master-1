package model;

public class PieceO extends Piece {

	public PieceO() {
		
		rectangle = new RotationRectangle(2, 2, new Coordinates(0, 0));
		
		blocks.add(new Coordinates(0, 0));
		blocks.add(new Coordinates(1, 0));
		blocks.add(new Coordinates(0, 1));
		blocks.add(new Coordinates(1, 1));
	}
	
	@Override
	public void rotate() {
		// Do nothing because there is no need to rotate this piece.
	}
}

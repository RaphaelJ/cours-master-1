package model;

public class PieceZ extends Piece {

	public PieceZ() {
		
		rectangle = new RotationRectangle(3, 3, new Coordinates(1, 1));

		blocks.add(new Coordinates(0, 1));
		blocks.add(new Coordinates(1, 1));
		blocks.add(new Coordinates(1, 2));
		blocks.add(new Coordinates(2, 2));
	}
}

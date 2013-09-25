package model;

public class PieceJ extends Piece {

	public PieceJ(Coordinates initialPosition) {
		super(initialPosition);
		
		rectangle = new RotationRectangle(3, 3, new Coordinates(1, 1));

		blocks.add(new Coordinates(0, 1));
		blocks.add(new Coordinates(1, 1));
		blocks.add(new Coordinates(2, 1));
		blocks.add(new Coordinates(2, 2));
	}
}

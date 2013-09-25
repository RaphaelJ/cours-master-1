package model.piece;

import model.Coordinates;

public class PieceS extends Piece {

	public PieceS(Coordinates initialPosition) {
		super(initialPosition);
		
		rectangle = new RotationRectangle(3, 3, new Coordinates(1, 1));

		blocks.add(new Coordinates(1, 1));
		blocks.add(new Coordinates(2, 1));
		blocks.add(new Coordinates(0, 2));
		blocks.add(new Coordinates(1, 2));
	}
}

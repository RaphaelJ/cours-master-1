package model.piece;

import model.Coordinates;

public class PieceI extends Piece {

	public PieceI(Coordinates initialPosition) {
		super(initialPosition);

		rectangle = new RotationRectangle(4, 4, new Coordinates(1, 1));

		blocks.add(new Coordinates(1, 0));
		blocks.add(new Coordinates(1, 1));
		blocks.add(new Coordinates(1, 2));
		blocks.add(new Coordinates(1, 3));
	}
}

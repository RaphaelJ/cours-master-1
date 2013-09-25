package model.piece;

import model.Coordinates;

public class PieceT extends Piece {

	public PieceT(Coordinates initialPosition) {
		super(initialPosition);
		
		rectangle = new RotationRectangle(3, 3, new Coordinates(1, 1));

		blocks.add(new Coordinates(0, 1));
		blocks.add(new Coordinates(1, 1));
		blocks.add(new Coordinates(2, 1));
		blocks.add(new Coordinates(1, 2));
	}
}

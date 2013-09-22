package controller;

import model.Piece;
import view.PieceView;

public class PieceController {

	private Piece piece;
	private PieceView view;
	
	public PieceController(Piece piece, PieceView view) {
		
		this.piece = piece;
		this.view = view;
	}
	
	public void rotatePiece() {
		
		piece.rotate();
	}
	
	public void printPiece() {
		
		view.display(piece.getBlocksPositions());
	}
}

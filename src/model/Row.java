package model;

import java.util.Arrays;

import model.piece.Piece;

public class Row {

	private int _width;
	private int _nbBlocks;
	private Piece[] _pieces;
	
	public Row(int width) {
		
		this._width = width;
		this._nbBlocks = 0;
		this._pieces = new Piece[width];
		
		Arrays.fill(this._pieces, null);
	}
	
	public Piece[] getPieces() {
		return this._pieces;
	}
	
	public Piece getPiece(int column) {
		return this._pieces[column];
	}
	
	public void setPiece(Piece piece, int column) {
		
		if(piece != null)
			this._nbBlocks++;
		else
			this._nbBlocks--;
		
		this._pieces[column] = piece;
	}
	
	public boolean isRowComplete() {
		return this._nbBlocks == this._width;
	}
}

package controller;

import model.Playfield;
import model.piece.Piece;

public class GameController {
        private Playfield grid;

        private Piece fallingPiece;
        private Piece nextPiece;

        public GameController(int width, int height)
        {
                this.grid = new Playfield(width, height);
        }

        public void rotatePiece()
        {
                fallingPiece.rotate();
        }
}

package view;

import java.awt.Rectangle;

import gameplay.*;
import model.Board;
import model.BoardListener;
import model.Row;
import model.piece.Piece;

/** Provides a view which prints the grid to the standard output. */
public class CLIView implements BoardListener {
    private GamePlay _game;

    public CLIView(GamePlay game)
    {
        this._game = game;
        game.getBoard().addListener(this);

        this.printBoard();
    }

    private void printBoard()
    {
        Row[] grid = this._game.getBoard().getGrid();
        int width = this._game.getBoard().getWidth();

        for (int i = 0; i < width + 1; i++)
            System.out.print("~~");
        System.out.println();

        for (Row row : grid) {
            System.out.print("!");
            for (Piece piece : row.getPieces()) {
            	if (piece != null)
                    System.out.print(" #");
                else
                    System.out.print("  ");
            }
            System.out.print(" !");

            System.out.println();
        }

        for (int i = 0; i < width + 1; i++)
            System.out.print("~~");
        System.out.println();
    }

    public void stateChange(Board.GameState newState)
    {
        switch (newState) {
        case INITIALIZED:
            this.printBoard();
            break;
        case GAMEOVER:
            System.out.println("Game over");
            break;
        case PAUSED:
        case RUNNING:
        	break;
        }
    }

    public void gridChange(Rectangle bounds)
    {
        this.printBoard();
    }

    public void clearedLines(int n)
    {
        System.out.println("You cleared " + n + " line(s)");
    }

    public void newPiece(Piece piece) {
    }
}

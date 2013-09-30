package view;

import model.Board;
import model.piece.Piece;

/** Provides a view which prints the grid to the standard output. */
public class CLIView implements Board.BoardListener {
    private Board _board;

    public CLIView(Board board)
    {
        this._board = board;

        this.printBoard();
    }

    private void printBoard()
    {
        Piece[][] grid = this._board.getGrid();
        int width = this._board.getWidth();

        for (int i = 0; i < width + 1; i++)
            System.out.print("~~");
        System.out.println();

        for (Piece[] line : grid) {
            System.out.print("!");
            for (Piece piece : line) {
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
    }

    public void gridChange()
    {
        this.printBoard();
    }

    public void clearedLines(int n)
    {
        System.out.println("You cleared " + n + " line(s)");
    }

    public void gameOver()
    {
        System.out.println("Game over");
    }

    public void reset()
    {
        this.printBoard();
    }
}

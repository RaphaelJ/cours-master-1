package view;

/** Provides a view which prints the grid to the standard output. */
public class PieceViewCLI extends PieceView {
    private Board _board;

    public PieceViewCLI(Board board)
    {
        this._board = board;
    }

    private void printBoard()
    {
        Piece[][] grid = this._board.getGrid();

        for (Piece[] line : grid) {
            for (Piece piece : line) {
                if (piece != null)
                    System.out.print("#");
                else
                    System.out.print(" ");
            }

            System.out.println();
        }
    }
    
    gridChange

    public void clearedLines(int n)
    {
        System.out.println("You cleared " + n + " line(s)");
    }

    public void gameOver()
    {
        System.out.println("Game over");
    }
}

package view;

import game.GameListener;
import game.GameManager;
import game.GameObserver;
import model.BoardSection;
import model.FullBoardSection;
import model.piece.Piece;

/** Provides a view which prints the grid to the standard output. */
public class CLIView implements GameListener {
    private GameObserver _game;

    public CLIView(GameObserver game)
    {
        this._game = game;
        game.addListener(this);

        this.printBoard();
    }

    private void printBoard()
    {
        FullBoardSection grid = this._game.getGrid();
        int width  = this._game.getGridWidth()
          , height = this._game.getGridHeight();

        for (int i = 0; i < width + 1; i++)
            System.out.print("~~");
        System.out.println();

        for (int i = 0; i < height; i++) {
            System.out.print("!");
            for (int j = 0; j < width; j++) {
                if (grid.get(i, j) != null)
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

    public void stateChanged(GameManager.GameState newState)
    {
        switch (newState) {
        case INITIALIZED:
            this.printBoard();
            break;
        case GAMEOVER:
            System.out.println("Game over");
            break;
        default:
        }
    }

    public void timeChanged(long elapsed) { }

    public void gridChange(BoardSection bounds)
    {
        this.printBoard();
    }

    public void newPiece(Piece current, Piece next)
    {
    }

    public void scoreChange(int newScore)
    {
    }

    public void levelChange(int newLevel)
    {
        System.out.println(
            "Next level : " + newLevel + " - Score : " + this._game.getScore()
        );
    }

    public void clockDelayChange(int newClockDelay)
    {
    }
}

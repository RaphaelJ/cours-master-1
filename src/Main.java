import model.Board;
import view.*;

public class Main {
    public static void main(String[] args)
    {
        Board board = new Board();
        new CLIView(board);
        new SwingView(board).run();
    }
}

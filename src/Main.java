import model.Board;
import view.*;

public class Main {
    public static void main(String[] args)
    {
        Board board = new Board();
        CLIView cliView = new CLIView(board);
    }
}

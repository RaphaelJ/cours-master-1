import controller.*;
import gameplay.*;
import model.Board;
import view.*;

public class Main {
    public static void main(String[] args)
    {
        GamePlay rules = new NintendoGameBoy();
        Board board = new Board(rules);

        //CLIView   cli = new CLIView(board);
        SwingView gui = new SwingView(board);
        //board.addView(cli); // Listen to board changes.
        board.addView(gui);

        // Controller which listen to GUI events and transmits them to the
        // game's board.
        GameController controller = new LocalController(board);
        gui.addController(controller);

        gui.run();
    }
}

import controller.*;
import gameplay.*;
import model.Board;
import view.*;

public class Main {
    public static void main(String[] args)
    {
        GamePlay game = new NintendoGameBoy(new Board());

        CLIView   cli = new CLIView(game);
        SwingView gui = new SwingView(game);

        // Controller which listen to GUI events and transmits them to the
        // game's manager.
        GameController controller = new LocalController(game);
        gui.addController(controller);

        gui.run();
    }
}

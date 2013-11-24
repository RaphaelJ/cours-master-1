import controller.*;
import gameplay.*;
import gameplay.multi.*;
import model.Board;
import util.random.*;
import view.*;

public class Main {
    public static void main(String[] args)
    {
        long commonSeed = new LCGRandom().getSeed();

        Board board1 = new Board(new LCGRandom(commonSeed))
            , board2 = new Board(new LCGRandom(commonSeed));

        GamePlayFactory innerGameplay = new NintendoGameBoyFactory();
//         DualGamePlay gameplay = new DualGamePlay(innerGameplay, board1, board2);
        DualGamePlay gameplay = new DualClassic(innerGameplay, board1, board2);
//         DualGamePlay gameplay = new DualCooperative(innerGameplay, board1, board2);

        GamePlay player1Gameplay = gameplay.getPlayer1GamePlay()
               , player2Gameplay = gameplay.getPlayer2GamePlay();

        board1.setGamePlay(player1Gameplay);
        board2.setGamePlay(player2Gameplay);

        CLIView   cli = new CLIView(player1Gameplay);

        SwingView gui1 = new SwingView(player1Gameplay)
                , gui2 = new SwingView(player2Gameplay);

        // Controller which listen to GUI events and transmits them to the
        // game's manager.
        gui1.addController(new LocalController(player1Gameplay));
        gui2.addController(new LocalController(player2Gameplay));

        gui1.run();
        gui2.run();
    }
}

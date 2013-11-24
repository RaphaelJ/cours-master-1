package gameplay;

import model.Board;

public class NintendoGameBoyFactory implements GamePlayFactory {
    public GamePlay construct(Board board)
    {
        return new NintendoGameBoy(board);
    }
}

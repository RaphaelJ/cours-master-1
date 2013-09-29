package model.piece;

public class PieceO extends Piece {
    public class PieceOFactory extends Piece.PieceFactory {
        public int extent = 2;

        public Piece construct(int currentState)
        {
            return new PieceO(currentState);
        }
    }

    // Shares the states between each instances of the Pieces.
    private static final states = { {
            { true, true },
            { true, true }
        }
    };
    protected final Boolean[][][] _states = states;

    private static final PieceOFactory factory = new PieceOFactory();
    protected final PieceFactory _factory = factory;

    public PieceO(int currentState)
    {
        super(currentState);
    }
}

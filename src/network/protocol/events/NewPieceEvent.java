package network.protocol.events;

import model.piece.*;

public class NewPieceEvent extends ObserverEvent {

    public final Piece currentPiece;
    public final Piece nextPiece;

    public NewPieceEvent(int playerId, Piece currentPiece, Piece nextPiece)
    {
        super(playerId);
        this.currentPiece = currentPiece;
        this.nextPiece    = nextPiece;
    }
}

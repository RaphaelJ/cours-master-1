package network.protocol.events;

import java.io.*;

import model.*;
import model.piece.*;

public class GridChangeEvent extends ObserverEvent {

    /** BoardSection which contains its pieces in an array so it can be
     * serialized and transmitted over the network. */
    public class SerializableBoardSection implements BoardSection,
                                                     Serializable {

        private final Piece[][] _section;

        private final int _x;
        private final int _y;
        private final int _width;
        private final int _height;

        /** Copies this given section content in the internal array. */
        public SerializableBoardSection(BoardSection section)
        {
            this._x      = section.getX();
            this._y      = section.getY();
            this._width  = section.getWidth();
            this._height = section.getHeight();

            this._section = new Piece[section.getHeight()][section.getWidth()];

            for (int y = 0; y < section.getHeight(); y++) {
                for (int x = 0; x < section.getWidth(); x++)
                    this._section[y][x] = section.get(y, x);
            }
        }

        public int getX()
        {
            return this._x;
        }

        public int getY()
        {
            return this._y;
        }

        public int getWidth()
        {
            return this._width;
        }

        public int getHeight()
        {
            return this._height;
        }

        public Piece get(int y, int x)
        {
            return this._section[y][x];
        }
    }

    public final SerializableBoardSection section;

    public GridChangeEvent(int playerId, BoardSection section)
    {
        super(playerId);
        this.section = new SerializableBoardSection(section);
    }
}

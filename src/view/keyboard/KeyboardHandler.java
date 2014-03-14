package view.keyboard;

import java.util.ArrayList;
import java.util.Set;

import game.*;

/** Given a KeySet, calls the corresponding methods of the controllers for each
 * pressed key. */
public class KeyboardHandler {

    private Set<Integer> _activeKeys;
    private KeySet _keySet;
    private GamePlayer _game;
    private boolean _enabled;

    public KeyboardHandler(Set<Integer> activeKeys, KeySet keySet,
                           GamePlayer game)
    {
        this._activeKeys = activeKeys;
        this._keySet = keySet;
        this._game = game;
        this._enabled = true;
    }

    public void checkKeys()
    {
        // Check keys only if the handler is enabled (if a human is playing)
        if(!this._enabled)
            return;

        if(_activeKeys.contains(_keySet.getKeyRotate()))
            this._game.rotate();

        if(_activeKeys.contains(_keySet.getKeyLeft()))
            this._game.moveLeft();

        if(_activeKeys.contains(_keySet.getKeyRight()))
            this._game.moveRight();

        if(_activeKeys.contains(_keySet.getKeySoftDrop()))
            this._game.softDrop();

        if(_activeKeys.contains(_keySet.getKeyHardDrop()))
            this._game.hardDrop();
    }

    public boolean isEnabled() {
        return this._enabled;
    }

    public void setEnabled(boolean enabled) {
        this._enabled = enabled;
    }
}

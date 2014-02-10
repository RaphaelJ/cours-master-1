package view;

import java.util.ArrayList;
import java.util.Set;

import controller.GameController;

/** Given a KeySet, calls the corresponding methods of the controllers for each
 * pressed key. */
public class KeyboardHandler {

    private Set<Integer> _activeKeys;
    private KeySet _keySet;
    private ArrayList<GameController> _controllers;

    public KeyboardHandler(Set<Integer> activeKeys, KeySet keySet,
                           ArrayList<GameController> controllers)
    {
        this._activeKeys = activeKeys;
        this._keySet = keySet;
        this._controllers = controllers;
    }

    public void checkKeys()
    {
        if(_activeKeys.contains(_keySet.getKeyRotate()))
            for(GameController controller : _controllers)
                controller.rotate();

        if(_activeKeys.contains(_keySet.getKeyLeft()))
            for(GameController controller : _controllers)
                controller.moveLeft();

        if(_activeKeys.contains(_keySet.getKeyRight()))
            for(GameController controller : _controllers)
                controller.moveRight();

        if(_activeKeys.contains(_keySet.getKeySoftDrop()))
            for(GameController controller : _controllers)
                controller.softDrop();

        if(_activeKeys.contains(_keySet.getKeyHardDrop()))
            for(GameController controller : _controllers)
                controller.hardDrop();
    }
}

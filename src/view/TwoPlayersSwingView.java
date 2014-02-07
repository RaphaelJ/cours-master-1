package view;

import gameplay.*;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.*;

import javax.swing.JFrame;

import controller.GameController;

public class TwoPlayersSwingView extends SwingView implements KeyListener {

    private GamePlay _game1, _game2;
    private GamePanel _panel1, _panel2;

    private ArrayList<GameController> _controllers1, _controllers2;
    private Set<Integer> _activeKeys = new HashSet<Integer>();

    public TwoPlayersSwingView(JFrame parent, GamePlay game1, GamePlay game2,
                                 boolean useImages)
    {
        super(parent, game1.getBoard());

        this._game1 = game1;
        this._game2 = game2;

        this._panel1 = new GamePanel(game1, useImages);
        this._panel2 = new GamePanel(game2, useImages);

        this._controllers1 = new ArrayList<GameController>();
        this._controllers2 = new ArrayList<GameController>();

        this.playPanel.add(this._panel1);
        this.playPanel.add(this._panel2);
        pack();

        this.addKeyListener(this);
    }

    public void addControllerPlayer1(GameController controller)
    {
        this._controllers1.add(controller);
    }

    public void addControllerPlayer2(GameController controller)
    {
        this._controllers2.add(controller);
    }

    @Override
    protected void newGame()
    {
        for (GameController controller : _controllers1)
            controller.newGame();
    }

    @Override
    protected long getElapsedTime()
    {
        return this._game1.getBoard().getElapsedTime();
    }

    @Override
    public void keyPressed(KeyEvent event)
    {
        _activeKeys.add(event.getKeyCode());

        if(_activeKeys.contains(KeyEvent.VK_P))
            for (GameController controller : this._controllers1)
                controller.pause();

        // Left player
        if(_activeKeys.contains(KeyEvent.VK_Q))
            for (GameController controller : this._controllers1)
                controller.moveLeft();

        if(_activeKeys.contains(KeyEvent.VK_D))
            for (GameController controller : this._controllers1)
                controller.moveRight();

        if(_activeKeys.contains(KeyEvent.VK_S))
            for (GameController controller : this._controllers1)
                controller.softDrop();

        if(_activeKeys.contains(KeyEvent.VK_A))
            for (GameController controller : this._controllers1)
                controller.hardDrop();

        if(_activeKeys.contains(KeyEvent.VK_Z))
            for (GameController controller : this._controllers1)
                controller.rotate();

        // Right player
        if(_activeKeys.contains(KeyEvent.VK_LEFT))
            for (GameController controller : this._controllers2)
                controller.moveLeft();

        if(_activeKeys.contains(KeyEvent.VK_RIGHT))
            for (GameController controller : this._controllers2)
                controller.moveRight();

        if(_activeKeys.contains(KeyEvent.VK_DOWN))
            for (GameController controller : this._controllers2)
                controller.softDrop();

        if(_activeKeys.contains(KeyEvent.VK_ENTER))
            for (GameController controller : this._controllers2)
                controller.hardDrop();

        if(_activeKeys.contains(KeyEvent.VK_UP))
            for (GameController controller : this._controllers2)
                controller.rotate();
    }

    @Override
    public void keyReleased(KeyEvent e) {
        _activeKeys.remove(e.getKeyCode());
    }

    @Override
    public void keyTyped(KeyEvent e) { }
}

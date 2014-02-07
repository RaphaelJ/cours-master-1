package view;

import gameplay.*;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.*;

import javax.swing.JFrame;

import controller.GameController;

public class SinglePlayerSwingView extends SwingView implements KeyListener {

    private GamePlay _game;
    private GamePanel _panel;

    private ArrayList<GameController> _controllers;

    public SinglePlayerSwingView(JFrame parent, GamePlay game, boolean useImages)
    {
        super(parent, game.getBoard());

        this._game = game;
        this._panel = new GamePanel(this, game, useImages);

        this._controllers = new ArrayList<GameController>();

        this.playPanel.add(this._panel);
        pack();

        this.addKeyListener(this);
    }

    public void addController(GameController controller)
    {
        this._controllers.add(controller);
    }

    @Override
    protected void newGame()
    {
        for (GameController controller : _controllers)
            controller.newGame();
    }

    @Override
    protected long getElapsedTime()
    {
        return this._game.getBoard().getElapsedTime();
    }

    public void keyPressed(KeyEvent event)
    {
        switch(event.getKeyCode()) {
        case KeyEvent.VK_P:
            for (GameController controller : this._controllers)
                controller.pause();
            break;

        case KeyEvent.VK_LEFT:
            for (GameController controller : this._controllers)
                controller.moveLeft();
            break;
        case KeyEvent.VK_RIGHT:
            for (GameController controller : this._controllers)
                controller.moveRight();
            break;
        case KeyEvent.VK_DOWN:
            for (GameController controller : this._controllers)
                controller.softDrop();
            break;
        case KeyEvent.VK_ENTER:
            for (GameController controller : this._controllers)
                controller.hardDrop();
            break;
        case KeyEvent.VK_UP:
            for (GameController controller : this._controllers)
                controller.rotate();
            break;
        }
    }

    public void keyReleased(KeyEvent e) { }

    public void keyTyped(KeyEvent e) { }
}

package view;

import gameplay.*;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.*;
import javax.swing.*;

import controller.GameController;

public class TwoPlayersSwingView extends SwingView implements KeyListener {

    private GamePlay _game1, _game2;
    private GamePanel _panel1, _panel2;

    private ArrayList<GameController> _controllers1, _controllers2;

    public TwoPlayersSwingView(GamePlay game1, GamePlay game2,
                                 boolean useImages)
    {
        super(game1.getBoard());

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
        switch(event.getKeyCode()) {
        case KeyEvent.VK_P:
            for (GameController controller : this._controllers1)
                controller.pause();
            break;

        // Left player
        case KeyEvent.VK_Q:
            for (GameController controller : this._controllers1)
                controller.moveLeft();
            break;
        case KeyEvent.VK_D:
            for (GameController controller : this._controllers1)
                controller.moveRight();
            break;
        case KeyEvent.VK_S:
            for (GameController controller : this._controllers1)
                controller.softDrop();
            break;
        case KeyEvent.VK_A:
            for (GameController controller : this._controllers1)
                controller.hardDrop();
            break;
        case KeyEvent.VK_Z:
            for (GameController controller : this._controllers1)
                controller.rotate();
            break;

        // Right player
        case KeyEvent.VK_LEFT:
            for (GameController controller : this._controllers2)
                controller.moveLeft();
            break;
        case KeyEvent.VK_RIGHT:
            for (GameController controller : this._controllers2)
                controller.moveRight();
            break;
        case KeyEvent.VK_DOWN:
            for (GameController controller : this._controllers2)
                controller.softDrop();
            break;
        case KeyEvent.VK_ENTER:
            for (GameController controller : this._controllers2)
                controller.hardDrop();
            break;
        case KeyEvent.VK_UP:
            for (GameController controller : this._controllers2)
                controller.rotate();
            break;
        }
    }

    @Override
    public void keyReleased(KeyEvent e) { }

    @Override
    public void keyTyped(KeyEvent e) { }
}

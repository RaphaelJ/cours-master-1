package view;

import gameplay.GamePlay;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import javax.swing.JFrame;

import controller.GameController;
import controller.LocalController;

public class MultiPlayerSwingView extends SwingView implements KeyListener {

	private ArrayList<GamePlay> _games;
    private ArrayList<GamePanel> _panels;
    private ArrayList<GameController> _controllers;

    private ArrayList<GameController> _controllers1, _controllers2;
    private Set<Integer> _activeKeys = new HashSet<Integer>();

    public MultiPlayerSwingView(JFrame parent, ArrayList<GamePlay> games,
			boolean useImages) {
    	
        super(parent, games.get(0).getBoard());

        this._games = games;
        this._panels = new ArrayList<>();
        this._controllers = new ArrayList<>();
        
        for(GamePlay game : games) {
        	
        	GamePanel panel = new GamePanel(this, game, useImages);
        	this._panels.add(panel);
        	
        	this.playPanel.add(panel);
        	
        	this._controllers.add(new LocalController(game));
        }

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
        return this._games.get(0).getBoard().getElapsedTime();
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

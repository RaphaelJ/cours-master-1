package view;

import gameplay.*;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.*;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

import controller.GameController;

public class SinglePlayerSwingView extends SwingView implements KeyListener {

    private GamePlay _game;
    private GamePanel _panel;

    private Configuration _config;
    private Set<Integer> _activeKeys;
    private ArrayList<GameController> _controllers;
    private KeyboardHandler _keyboardHandler;

    public SinglePlayerSwingView(JFrame parent, GamePlay game,
                                 Configuration config)
    {
        super(parent, game.getBoard());

        this._game = game;
        this._panel = new GamePanel(this, game, config);

        this._config = config;
        this._activeKeys = new HashSet<>();
        this._controllers = new ArrayList<GameController>();
        this._keyboardHandler = new KeyboardHandler(this._activeKeys,
            this._config.getKeySet(0), this._controllers
        );

        this._panel.setKeyboardHandler(this._keyboardHandler);

        initComponents();
    }

    private void initComponents()
    {
        this.playPanel.add(this._panel);

        this.addKeyListener(this);
        this.addWindowListener(new WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt){
                    _game.stop();
            }
        });

        this.pack();
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

    @Override
    public void keyPressed(KeyEvent event)
    {
        _activeKeys.add(event.getKeyCode());

        // Use the first player to propagate the pause event to all controllers.
        if(_activeKeys.contains(KeyEvent.VK_P))
            for (GameController controller : this._controllers)
                controller.pause();

        _keyboardHandler.checkKeys();
    }

    @Override
    public void keyReleased(KeyEvent e) {
        _activeKeys.remove(e.getKeyCode());
    }

    public void keyTyped(KeyEvent e) { }

    @Override
    public void gameOver() {
        int choice = 0;
        choice = JOptionPane.showConfirmDialog(
            this,
            "Would you like to retry ?",
            "Game Over",
            JOptionPane.YES_NO_OPTION);

        if(choice == 0)
            newGame();
        else
            this.dispatchEvent(new WindowEvent(this,
                            WindowEvent.WINDOW_CLOSING));
    }
}

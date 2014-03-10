package view;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.*;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

import model.config.LocalConfig;
import gameplay.*;

public class SinglePlayerSwingView extends SwingView implements KeyListener {

    private GamePlay _game;
    private GamePanel _panel;

    private LocalConfig _config;
    private Set<Integer> _activeKeys;
    private KeyboardHandler _keyboardHandler;

    public SinglePlayerSwingView(JFrame parent, GamePlay game,
                                 LocalConfig config)
    {
        super(parent);

        this._game = game;
        game.addListener(this);

        this._panel = new GamePanel(this, game, config);

        this._config = config;
        this._activeKeys = new HashSet<Integer>();
        this._keyboardHandler = new KeyboardHandler(
            this._activeKeys, this._config.getKeySet(0), game
        );

        this._panel.setKeyboardHandler(this._keyboardHandler);

        initComponents();
    }

    private void initComponents()
    {
        this.gamePanel.add(this._panel);

        this.addKeyListener(this);
        this.addWindowListener(new WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt){
                _game.stop();
            }
        });

        this.pack();
    }

    @Override
    protected void newGame()
    {
        this._game.newGame();
    }

    @Override
    public void keyPressed(KeyEvent event)
    {
        _activeKeys.add(event.getKeyCode());

        if (_activeKeys.contains(KeyEvent.VK_P))
            this._game.pause();

        _keyboardHandler.checkKeys();
    }

    @Override
    public void keyReleased(KeyEvent e) {
        _activeKeys.remove(e.getKeyCode());
    }

    public void keyTyped(KeyEvent e) { }

    @Override
    public void stateChanged(GamePlay.GameState newState)
    {
        if (newState == GamePlay.GameState.GAMEOVER) {
            int choice = JOptionPane.showConfirmDialog(
                this, "Would you like to retry ?", "Game Over",
                JOptionPane.YES_NO_OPTION
            );

            if (choice == 0)
                this.newGame();
            else {
                this.dispatchEvent(
                    new WindowEvent(this, WindowEvent.WINDOW_CLOSING)
                );
            }
        }
    }
}

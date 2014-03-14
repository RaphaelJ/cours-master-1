package view;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.*;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

import model.config.LocalConfig;
import game.*;
import view.keyboard.*;
import view.panel.*;

public class SinglePlayerSwingView extends SwingView implements KeyListener {

    private GamePlayer _game;
    private PlayerGamePanel _panel;

    private LocalConfig _config;
    private Set<Integer> _activeKeys;
    private KeyboardHandler _keyboardHandler;

    public SinglePlayerSwingView(JFrame parent, GamePlayer game,
                                 LocalConfig config)
    {
        super(parent, game);

        this._game = game;

        this._config = config;
        this._activeKeys = new HashSet<Integer>();
        this._keyboardHandler = new KeyboardHandler(
            this._activeKeys, this._config.getKeySet(0), game
        );

        this._panel = new PlayerGamePanel(
            this, game, config, this._keyboardHandler
        );

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
    public void stateChanged(GamePlayer.GameState newState)
    {
        if (newState == GamePlayer.GameState.GAMEOVER) {
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

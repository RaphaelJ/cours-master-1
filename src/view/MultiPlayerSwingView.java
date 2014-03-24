package view;


import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

import game.*;
import game.multi.*;
import model.config.LocalConfig;
import network.*;
import view.keyboard.KeyboardHandler;
import view.panel.*;

public class MultiPlayerSwingView extends SwingView implements KeyListener {

    private GameManager _game;
    private ArrayList<GameObserver> _games = new ArrayList<GameObserver>();
    private ArrayList<ObserverGamePanel> _panels
        = new ArrayList<ObserverGamePanel>();

    private LocalConfig _config;
    private Set<Integer> _activeKeys = new HashSet<Integer>();
    private ArrayList<KeyboardHandler> _keyboardHandlers
        = new ArrayList<KeyboardHandler>();

    /** Starts the interface for a multiplayer game. */
    public MultiPlayerSwingView(JFrame parent, MultiGame multiGame,
                                LocalConfig config, boolean useImages)
    {
        super(parent, multiGame);

        this._game = multiGame;
        this._config = config;

        initMultiComponents(multiGame);
    }

    /** Starts the interface for an online multiplayer game. */
    public MultiPlayerSwingView(JFrame parent, GameClient client,
                                LocalConfig config, boolean useImages)
    {
        super(parent, client);

        this._game = client;
        this._config = config;

        initOnlineComponents(client);
    }

    private void initMultiComponents(MultiGame multiGame)
    {
        // Creates a PlayerGamePanel for each player with an associed keyboard
        // handler.
        for (int i = 0; i < this._config.getNbPlayersMulti(); i++) {
            GamePlayer game = multiGame.getPlayerGame(i);

            KeyboardHandler handler = new KeyboardHandler(
                this._activeKeys, this._config.getKeySet(i), game
            );

            this._keyboardHandlers.add(handler);

            this._games.add(game);
            PlayerGamePanel panel = new PlayerGamePanel(
                this, game, this._config, handler
            );
            this._panels.add(panel);
            this.gamePanel.add(panel);
        }

        this.initComponents();
    }

    private void initOnlineComponents(GameClient client)
    {
        ArrayList<GameObserverProxy> players = client.getPlayers();

        // Creates a PlayerGamePanel for the first player.
        GamePlayer player = (GamePlayerProxy) players.get(0);
        KeyboardHandler handler = new KeyboardHandler(
            this._activeKeys, this._config.getKeySet(0), player
        );
        this._keyboardHandlers.add(handler);

        this._games.add(player);
        PlayerGamePanel playerPanel = new PlayerGamePanel(
            this, player, this._config, handler
        );
        this._panels.add(playerPanel);
        this.gamePanel.add(playerPanel);

        // Creates ObserverGamePanels for opponents.
        for (int i = 1; i < players.size(); i++) {
            GameObserverProxy observer = players.get(i);

            this._games.add(observer);
            ObserverGamePanel panel = new ObserverGamePanel(
                this, observer, this._config
            );
            this._panels.add(panel);
            this.gamePanel.add(panel);
        }

        this.initComponents();
    }

    private void initComponents()
    {
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

        for (KeyboardHandler handler : this._keyboardHandlers)
            handler.checkKeys();
    }

    @Override
    public void keyReleased(KeyEvent e)
    {
        _activeKeys.remove(e.getKeyCode());
    }

    @Override
    public void keyTyped(KeyEvent e) { }

    @Override
    public void stateChanged(GameManager.GameState newState)
    {
        if (newState == GameManager.GameState.GAMEOVER) {
            // Seeks the player with the top score.
            int numWinner = 0;
            int scoreWinner = 0;

            int i = 0;
            for (GameObserver game : this._games) {
                int score = game.getScore();

                if (score > scoreWinner) {
                    numWinner = i;
                    scoreWinner = score;
                }

                i++;
            }

            JOptionPane.showMessageDialog(
                this,
                "Player " + (numWinner+1) + " wins the game with " + scoreWinner
                + " points !",
                "Game Over",
                JOptionPane.INFORMATION_MESSAGE
            );

            int choice = JOptionPane.showConfirmDialog(
                this, "Would you like to retry ?", "Game Over",
                JOptionPane.YES_NO_OPTION
            );

            if (choice == 0)
                newGame();
            else {
                this.dispatchEvent(
                    new WindowEvent(this, WindowEvent.WINDOW_CLOSING)
                );
            }
        }
    }
}

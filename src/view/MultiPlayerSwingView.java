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
import game.multi.*;
import view.keyboard.*;
import view.panel.*;

public class MultiPlayerSwingView extends SwingView implements KeyListener {

    private MultiGame _multiGame;
    private ArrayList<PlayerGamePanel> _panels;

    private LocalConfig _config;
    private Set<Integer> _activeKeys;
    private ArrayList<KeyboardHandler> _keyboardHandlers;

    public MultiPlayerSwingView(JFrame parent, MultiGame multiGame,
                                LocalConfig config, boolean useImages)
    {
        super(parent, multiGame);

        this._multiGame = multiGame;
        multiGame.addListener(this);

        this._panels = new ArrayList<PlayerGamePanel>();

        this._config = config;
        this._activeKeys = new HashSet<Integer>();
        this._keyboardHandlers = new ArrayList<KeyboardHandler>();

        initComponents();
    }

    private void initComponents()
    {
        // Creates a game panel for each player with an associed keyboard
        // handler.
        for (int i = 0; i < this._config.getNbPlayersMulti(); i++) {
            GamePlayer game = this._multiGame.getPlayerGame(i);

            KeyboardHandler handler = new KeyboardHandler(
                this._activeKeys, this._config.getKeySet(i), game
            );

            this._keyboardHandlers.add(handler);

            PlayerGamePanel panel = new PlayerGamePanel(
                this, game, this._config, handler
            );
            this._panels.add(panel);
            this.gamePanel.add(panel);
        }

        this.addKeyListener(this);
        this.addWindowListener(new WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt){
                _multiGame.stop();
            }
        });

        this.pack();
    }

    @Override
    protected void newGame()
    {
        this._multiGame.newGame();
    }

    @Override
    public void keyPressed(KeyEvent event)
    {
        _activeKeys.add(event.getKeyCode());

        if (_activeKeys.contains(KeyEvent.VK_P))
            this._multiGame.pause();

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
            for (GamePlayer game : this._multiGame.getGames()) {
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

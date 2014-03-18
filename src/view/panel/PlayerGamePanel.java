package view.panel;

import game.GamePlayer;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import model.config.Config;
import view.SwingView;
import view.keyboard.KeyboardHandler;

/** Displays a game and listen to key events from the user to change the game's
 * state. */
public class PlayerGamePanel extends ObserverGamePanel implements ItemListener {

	private static final long serialVersionUID = 4213895928311919154L;

	private KeyboardHandler _keyboardHandler;

    private JPanel _autoPlayerPanel;
    private JLabel _autoPlayerLabel;
    private JCheckBox _autoPlayerCheckBox;

    private GamePlayer _game;

    public PlayerGamePanel(SwingView parent, GamePlayer game, Config config,
                           KeyboardHandler handler)
    {
        super(parent, game, config);

        this._game = game;

        this._keyboardHandler = handler;

        this.initComponents();
    }

    private void initComponents()
    {
        this._autoPlayerPanel = new JPanel();
        this._autoPlayerLabel = new JLabel("Let the computer play :");
        this._autoPlayerCheckBox = new JCheckBox();
        this._autoPlayerCheckBox.addItemListener(this);

        this._autoPlayerPanel.setLayout(new FlowLayout());
        this._autoPlayerPanel.add(this._autoPlayerLabel);
        this._autoPlayerPanel.add(this._autoPlayerCheckBox);

        this.add(this._autoPlayerPanel, BorderLayout.SOUTH);
    }

    @Override
    public void itemStateChanged(ItemEvent e)
    {
        Object source = e.getItemSelectable();

        if (source == this._autoPlayerCheckBox) {
            // Start the AI
            if(e.getStateChange() == ItemEvent.SELECTED)
                this.startAutoPlayer();

            // Stop the AI
            else if(e.getStateChange() == ItemEvent.DESELECTED)
                this.stopAutoPlayer();

            /* Request focus to the frame so it can still receive the key
             * events.
             */
            this._parent.requestFocus();
        }
    }

    private void startAutoPlayer()
    {
        /* Disable the KeyboardHandler so it's not possible to control the game
         * while the AI is playing.
         */
        if (this._keyboardHandler != null)
            this._keyboardHandler.setEnabled(false);

        this._game.setAI(true);
    }

    private void stopAutoPlayer()
    {
        // Enable the KeyboardHandler to give the hand back to the human player.
        if (this._keyboardHandler != null)
            this._keyboardHandler.setEnabled(true);

        this._game.setAI(false);
    }
}
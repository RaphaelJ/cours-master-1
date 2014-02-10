package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import ai.*;
import gameplay.GamePlay;
import gameplay.GamePlayListener;
import model.Board;
import model.Board.GameState;
import model.BoardListener;
import model.Row;
import model.piece.Piece;
import view.piece.PieceViewModel;

public class GamePanel extends JPanel
        implements BoardListener, GamePlayListener, ItemListener {

    private SwingView _parent;

    private JPanel _playPanel;

    private JLabel _score;
    private JLabel _level;
    private JPanel _nextPiecePanel;

    private JPanel _autoPlayerPanel;
    private JLabel _autoPlayerLabel;
    private JCheckBox _autoPlayerCheckBox;

    private GamePlay _game;
    private boolean _useImages;
    private KeyboardHandler _keyboardHandler;
    
    private ArtificialIntelligence _ai;

    public GamePanel(SwingView parent, GamePlay game, boolean useImages)
    {
        this._parent = parent;
        this._game = game;
        game.addListener(this);
        game.getBoard().addListener(this);

        this._useImages = useImages;
        this._keyboardHandler = null;

        initComponents();

        this._ai = new ArtificialIntelligence(game, 1, 1, 4);
    }

    private void initComponents()
    {
        this._playPanel = new JPanel();
        JPanel rightPanel = new JPanel();

        JLabel scoreTitle = new JLabel("Score :");
        this._score = new JLabel(Integer.toString(this._game.getScore()));

        JLabel levelTitle = new JLabel("Level :");
        this._level = new JLabel(Integer.toString(this._game.getLevel()));

        this._nextPiecePanel = new JPanel();

        this._autoPlayerPanel = new JPanel();
        this._autoPlayerLabel = new JLabel("Let the computer play :");
        this._autoPlayerCheckBox = new JCheckBox();
        this._autoPlayerCheckBox.addItemListener(this);

        this._playPanel.setBackground(new java.awt.Color(255, 255, 255));
        this._playPanel.setPreferredSize(
            new Dimension(
                this._game.getBoard().getWidth() * PieceViewModel.TILES_SIZE,
                this._game.getBoard().getHeight() * PieceViewModel.TILES_SIZE
            )
        );

        rightPanel.setBorder(
            BorderFactory.createLineBorder(new Color(0, 0, 0))
        );

        this._nextPiecePanel.setBackground(new java.awt.Color(255, 255, 255));
        this._nextPiecePanel.setPreferredSize(
            new Dimension(
                4 * PieceViewModel.TILES_SIZE, 4 * PieceViewModel.TILES_SIZE
            )
        );

        rightPanel.setLayout(new BoxLayout(rightPanel, BoxLayout.Y_AXIS));
        rightPanel.add(scoreTitle);
        rightPanel.add(this._score);
        rightPanel.add(levelTitle);
        rightPanel.add(this._level);
        rightPanel.add(this._nextPiecePanel);

        this._autoPlayerPanel.setLayout(new FlowLayout());
        this._autoPlayerPanel.add(this._autoPlayerLabel);
        this._autoPlayerPanel.add(this._autoPlayerCheckBox);

        this.setLayout(new BorderLayout());
        this.add(this._playPanel, BorderLayout.CENTER);
        this.add(rightPanel, BorderLayout.EAST);
        this.add(this._autoPlayerPanel, BorderLayout.SOUTH);
    }

    public void gridChange(Rectangle bounds)
    {
        // Update the grid
        Row[] grid = this._game.getBoard().getGrid();
        Graphics g = this._playPanel.getGraphics();

        for (int i = bounds.y; i < bounds.y + bounds.height; i++) {
            Row row = grid[i];
            for (int j = bounds.x; j < bounds.x + bounds.width; j++) {
                Piece piece = row.getPiece(j);

                if (piece != null) {
                    try {
                        PieceViewModel pvm = new PieceViewModel(
                            piece, this._useImages
                        );
                        pvm.drawTexture(
                            g, j * PieceViewModel.TILES_SIZE,
                            i * PieceViewModel.TILES_SIZE, this
                        );
                    } catch (Exception e) { // Unable to load the tile.
                    }
                } else {
                    g.setColor(Color.WHITE);
                    g.fillRect(
                        j * PieceViewModel.TILES_SIZE,
                        i * PieceViewModel.TILES_SIZE,
                        PieceViewModel.TILES_SIZE,
                        PieceViewModel.TILES_SIZE
                    );
                }
            }
        }

        g.finalize();
    }

    public void stateChange(GameState newState)
    {
        switch (newState) {
        case INITIALIZED:
            this.cleanBoards();
            break;
        case RUNNING:
            // Redraws the entire grid.
            Board board = this._game.getBoard();
            this.gridChange(
                new Rectangle(0, 0, board.getWidth(), board.getHeight())
            );
            this.newPiece(board.getCurrentPiece(), board.getNextPiece());
            break;
        case PAUSED:
            this.cleanBoards();
            this.drawString("Game paused");
            break;
        case GAMEOVER:
            this._parent.gameOver();
            break;
        case STOPPED:
            break;
        }
    }

    /** Draws the next piece in its right panel. */
    public void newPiece(Piece piece, Piece newPiece)
    {
        Graphics g = this._nextPiecePanel.getGraphics();
        PieceViewModel pvm = new PieceViewModel(newPiece, this._useImages);

        int dimension = newPiece.getFactory().getExtent();
        boolean[][] state = newPiece.getCurrentState();

        // Erases the next piece panel
        g.setColor(Color.WHITE);
        g.fillRect(
            0, 0, PieceViewModel.TILES_SIZE * 4, PieceViewModel.TILES_SIZE * 4
        );

        // Draws the next piece at the center of the panel.
        int offset = (PieceViewModel.TILES_SIZE * 4 - PieceViewModel.TILES_SIZE
                                                * dimension) / 2;
        for (int i = 0; i < dimension; i++)
            for (int j = 0; j < dimension; j++)
                if (state[i][j])
                    try {
                        pvm.drawTexture(
                            g, offset + j * PieceViewModel.TILES_SIZE,
                            offset + i * PieceViewModel.TILES_SIZE, this
                        );
                    } catch (Exception e) { // Unable to load the tile.
                        System.err.println(e.getMessage());
                    }

        g.finalize();
    }

    public void scoreChange(int newScore)
    {
        this._score.setText(Integer.toString(newScore));
    }

    public void levelChange(int newLevel)
    {
        this._level.setText(Integer.toString(newLevel));
    }

    public void speedChange(int newClockSpeed) { }

    /** Removes every drawing for the board and the next piece panels. */
    private void cleanBoards()
    {
        // Hides the board.
        Graphics g = this._playPanel.getGraphics();
        g.setColor(Color.WHITE);
        g.fillRect(
            0, 0, this._game.getBoard().getWidth() * PieceViewModel.TILES_SIZE,
            this._game.getBoard().getHeight() * PieceViewModel.TILES_SIZE
        );

        // Hides the next piece panel.
        g = this._nextPiecePanel.getGraphics();
        g.setColor(Color.WHITE);
        g.fillRect(
            0, 0, PieceViewModel.TILES_SIZE * 4, PieceViewModel.TILES_SIZE * 4
        );

        g.finalize();
    }

    /** Draws the text at the center of the board. */
    private void drawString(String text)
    {
        Graphics g = this._playPanel.getGraphics();
        FontMetrics metrics = g.getFontMetrics();

        int width  = metrics.stringWidth(text)
          , height = metrics.getHeight();

        g.drawString(
            text,
            (this._game.getBoard().getWidth()
             * PieceViewModel.TILES_SIZE - width) / 2,
            (this._game.getBoard().getHeight() 
             * PieceViewModel.TILES_SIZE - height) / 2
        );

        g.finalize();
    }

    @Override
    public void itemStateChanged(ItemEvent e)
    {
        Object source = e.getItemSelectable();

        if(source == this._autoPlayerCheckBox) {
            if(e.getStateChange() == ItemEvent.SELECTED)
                this.startAutoPlayer();
            else if(e.getStateChange() == ItemEvent.DESELECTED)
                this.stopAutoPlayer();

            this._parent.requestFocus();
        }
    }

    private void startAutoPlayer()
    {
    	if(this._keyboardHandler != null)
			this._keyboardHandler.setEnabled(false);
    	
        this._ai.setActive(true);
    }

    private void stopAutoPlayer()
    {
    	if(this._keyboardHandler != null)
			this._keyboardHandler.setEnabled(true);
    	
        this._ai.setActive(false);
    }
    
    public void setKeyboardHandler(KeyboardHandler handler) {
    	this._keyboardHandler = handler;
    }
}

package view.panel;

import game.GameListener;
import game.GameManager;
import game.GameObserver;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.Graphics;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;

import model.BoardSection;
import model.config.Config;
import model.piece.Piece;
import view.SwingView;
import view.piece.PieceViewModel;

/** Provides a base-class to display a player's game in the interface.
 * Is extended by PlayerGamePanel (for games on which the user has control). */
public class ObserverGamePanel extends JPanel implements GameListener {

	private static final long serialVersionUID = -8912831479539837009L;

	protected SwingView _parent;

    private JPanel _playPanel;

    private JLabel _score;
    private JLabel _level;
    private JPanel _nextPiecePanel;

    private GameObserver _game;
    private Config _config;

    public ObserverGamePanel(SwingView parent, GameObserver game, Config config)
    {
        this._parent = parent;

        this._game = game;
        game.addListener(this);

        this._config = config;

        initComponents();
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

        this._playPanel.setBackground(new java.awt.Color(255, 255, 255));
        this._playPanel.setPreferredSize(
            new Dimension(
                this._game.getGridWidth()  * PieceViewModel.TILES_SIZE,
                this._game.getGridHeight() * PieceViewModel.TILES_SIZE
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

        this.setLayout(new BorderLayout());
        this.add(this._playPanel, BorderLayout.CENTER);
        this.add(rightPanel, BorderLayout.EAST);
    }

    public void gridChange(BoardSection section)
    {
        Graphics g = this._playPanel.getGraphics();

        for (int i = 0; i < section.getHeight(); i++) {
            int y = i + section.getY();

            for (int j = 0; j < section.getWidth(); j++) {
                Piece piece = section.get(i, j);
                int x = j + section.getX();

                if (piece != null) {
                    try {
                        PieceViewModel pvm = new PieceViewModel(
                            piece, this._config.isUseImages()
                        );
                        pvm.drawTexture(
                            g, x * PieceViewModel.TILES_SIZE,
                            y * PieceViewModel.TILES_SIZE, this
                        );
                    } catch (Exception e) { // Unable to load the tile.
                    }
                } else {
                    g.setColor(Color.WHITE);
                    g.fillRect(
                        x * PieceViewModel.TILES_SIZE,
                        y * PieceViewModel.TILES_SIZE,
                        PieceViewModel.TILES_SIZE, PieceViewModel.TILES_SIZE
                    );
                }
            }
        }

        g.finalize();
    }

    public void stateChanged(GameManager.GameState newState)
    {
        switch (newState) {
        case INITIALIZED:
            this.cleanBoards();
            break;
        case RUNNING:
            // Redraws the entire grid.
            this.gridChange(this._game.getGrid());
            this.newPiece(
                this._game.getCurrentPiece(), this._game.getNextPiece()
            );
            break;
        case PAUSED:
            this.cleanBoards();
            this.drawString("Game paused");
            break;
        default:
        }
    }

    public void timeChanged(long elapsed) { }

    /** Draws the next piece in its right panel. */
    public void newPiece(Piece piece, Piece newPiece)
    {
        Graphics g = this._nextPiecePanel.getGraphics();
        PieceViewModel pvm = new PieceViewModel(
            newPiece, this._config.isUseImages()
        );

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
        for (int i = 0; i < dimension; i++) {
            for (int j = 0; j < dimension; j++) {
                if (state[i][j])
                    try {
                        pvm.drawTexture(
                            g, offset + j * PieceViewModel.TILES_SIZE,
                            offset + i * PieceViewModel.TILES_SIZE, this
                        );
                    } catch (Exception e) { // Unable to load the tile.
                        System.err.println(e.getMessage());
                    }
            }
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

    public void clockDelayChange(int delay) { }

    /** Removes every drawing for the board and the next piece panels. */
    private void cleanBoards()
    {
        // Hides the board.
        Graphics g = this._playPanel.getGraphics();
        g.setColor(Color.WHITE);
        g.fillRect(
            0, 0, this._game.getGridWidth() * PieceViewModel.TILES_SIZE,
            this._game.getGridHeight() * PieceViewModel.TILES_SIZE
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
            (this._game.getGridWidth()
             * PieceViewModel.TILES_SIZE - width) / 2,
            (this._game.getGridHeight() 
             * PieceViewModel.TILES_SIZE - height) / 2
        );

        g.finalize();
    }
}

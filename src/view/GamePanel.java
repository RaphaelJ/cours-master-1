package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Rectangle;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;

import gameplay.GamePlay;
import gameplay.GamePlayListener;
import model.Board.GameState;
import model.BoardListener;
import model.Row;
import model.piece.Piece;
import view.piece.PieceViewModel;

public class GamePanel extends JPanel 
        implements BoardListener, GamePlayListener {

    private JPanel _playPanel;

    private JLabel _score;
    private JLabel _level;
    private JPanel _nextPiecePanel;

    private GamePlay _game;
    private boolean _useImages;

    public GamePanel(GamePlay game, boolean useImages)
    {
        this._game = game;
        game.addListener(this);
        game.getBoard().addListener(this);

        this._useImages = useImages;

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

        this.setLayout(new BorderLayout());
        this.add(this._playPanel, BorderLayout.CENTER);
        this.add(rightPanel, BorderLayout.EAST);
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
            this.gridChange(
                new Rectangle(0, 0, this._game.getBoard().getWidth(),
                this._game.getBoard().getHeight())
            );
            this.newPiece(this._game.getBoard().getNextPiece());
            break;
        case PAUSED:
            this.cleanBoards();
            this.drawString("Game paused");
            break;
        case GAMEOVER:
            this.drawString("Game over !");
            break;
        }
    }

    public void newPiece(Piece piece)
    {
        Graphics g = this._nextPiecePanel.getGraphics();
        PieceViewModel pvm = new PieceViewModel(piece, this._useImages);

        int dimension = piece.getFactory().getExtent();
        boolean[][] state = piece.getCurrentState();

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
                if (state[i][j]) {
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
}

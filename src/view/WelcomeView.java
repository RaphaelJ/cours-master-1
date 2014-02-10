package view;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import controller.LocalController;
import gameplay.GamePlay;
import gameplay.GamePlayFactory;
import gameplay.NintendoGameBoy;
import gameplay.NintendoGameBoyFactory;
import gameplay.multi.MultiClassic;
import gameplay.multi.MultiCooperative;
import gameplay.multi.MultiGamePlay;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

import model.Board;
import util.random.LCGRandom;

public class WelcomeView extends javax.swing.JFrame {

    private JPanel jPanelButtons;

    private JButton jButtonCoop;
    private JButton jButtonMultiClassic;
    private JButton jButtonMultiSimple;
    private JButton jButtonOptions;
    private JButton jButtonSolo;
    private JButton jButtonExit;

    private Configuration _config;

    public WelcomeView()
    {
        super("Tetris MVC");

        this._config = new Configuration();

        initComponents();
    }

    private void initComponents()
    {
        this.getContentPane().setLayout(new GridBagLayout());

        jPanelButtons = new JPanel();

        jButtonSolo = new JButton();
        jButtonMultiSimple = new JButton();
        jButtonMultiClassic = new JButton();
        jButtonCoop = new JButton();
        jButtonOptions = new JButton();
        jButtonExit = new JButton();

        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);

        jButtonSolo.setText("Solo mode");
        jButtonSolo.setAlignmentX(CENTER_ALIGNMENT);
        jButtonSolo.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jButtonSoloActionPerformed(evt);
            }
        });

        jButtonMultiSimple.setText("Multi simple mode");
        jButtonMultiSimple.setAlignmentX(CENTER_ALIGNMENT);
        jButtonMultiSimple.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jButtonMultiSimpleActionPerformed(evt);
            }
        });

        jButtonMultiClassic.setText("Multi classic mode");
        jButtonMultiClassic.setAlignmentX(CENTER_ALIGNMENT);
        jButtonMultiClassic.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jButtonMultiClassicActionPerformed(evt);
            }
        });

        jButtonCoop.setText("Cooperative mode");
        jButtonCoop.setAlignmentX(CENTER_ALIGNMENT);
        jButtonCoop.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jButtonCoopActionPerformed(evt);
            }
        });

        jButtonOptions.setText("Options");
        jButtonOptions.setAlignmentX(CENTER_ALIGNMENT);
        jButtonOptions.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jButtonOptionsActionPerformed(evt);
            }
        });

        jButtonExit.setText("Exit");
        jButtonExit.setAlignmentX(CENTER_ALIGNMENT);
        jButtonExit.addActionListener(new ActionListener() {
                        public void actionPerformed(ActionEvent e) {
                                        setVisible(false);
                                        System.exit(0);
                        }
                });

        GridLayout layout = new GridLayout(6, 1);
        layout.setVgap(10);
        jPanelButtons.setLayout(layout);
        jPanelButtons.add(jButtonSolo);
        jPanelButtons.add(jButtonMultiSimple);
        jPanelButtons.add(jButtonMultiClassic);
        jPanelButtons.add(jButtonCoop);
        jPanelButtons.add(jButtonOptions);
        jPanelButtons.add(jButtonExit);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(10, 10, 10, 10);

        this.getContentPane().add(jPanelButtons, gbc);

        pack();
    }

    private void jButtonSoloActionPerformed(ActionEvent evt) {
        this.setVisible(false);

        Board board = new Board(this._config.getBoardWidth(),
                        this._config.getBoardHeight());
        GamePlay game = new NintendoGameBoy(board);

        board.setGamePlay(game);

        SinglePlayerSwingView gui = new SinglePlayerSwingView(this, game,
                        this._config);

        gui.addController(new LocalController(game));

        gui.run();
    }

    private void jButtonMultiSimpleActionPerformed(ActionEvent evt) {
        ArrayList<Board> boards = new ArrayList<Board>();

        for(int i = 0; i < this._config.getNbPlayersMulti(); i++) {
            boards.add(new Board(new LCGRandom(),
                this._config.getBoardWidth(),
                this._config.getBoardHeight())
            );

            // Sleep a little bit to avoid having the same seed for all boards.
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        GamePlayFactory innerGameplay = new NintendoGameBoyFactory();
        MultiGamePlay game = new MultiGamePlay(innerGameplay, boards);

        startMultiPlayersGame(game);
    }

    private void jButtonMultiClassicActionPerformed(ActionEvent evt)
    {
        ArrayList<Board> boards = new ArrayList<Board>();
        long commonSeed = new LCGRandom().getSeed();

        for(int i = 0; i < this._config.getNbPlayersMulti(); i++)
                boards.add(new Board(new LCGRandom(commonSeed),
                                this._config.getBoardWidth(),
                                this._config.getBoardHeight()));

        GamePlayFactory innerGameplay = new NintendoGameBoyFactory();

        int posHole = new LCGRandom().nextInt(boards.get(0).getWidth());
        MultiGamePlay game = new MultiClassic(
                        innerGameplay,
                        boards,
                        posHole);

        startMultiPlayersGame(game);
    }

    private void jButtonCoopActionPerformed(ActionEvent evt) {
        ArrayList<Board> boards = new ArrayList<Board>();

        for(int i = 0; i < this._config.getNbPlayersMulti(); i++) {
                boards.add(new Board(this._config.getBoardWidth(),
                                this._config.getBoardHeight()));

                // Sleep a little bit to avoid having the same seed for all boards.
                try {
                    Thread.sleep(100);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
        }

        GamePlayFactory innerGameplay = new NintendoGameBoyFactory();
        MultiCooperative game = new MultiCooperative(innerGameplay, boards);

        startMultiPlayersGame(game);
    }

    private void startMultiPlayersGame(MultiGamePlay game)
    {
        this.setVisible(false);

        ArrayList<GamePlay> games = new ArrayList<GamePlay>();

        for(int i = 0; i < this._config.getNbPlayersMulti(); i++) {
                GamePlay gameplay = game.getPlayerGamePlay(i);
                games.add(gameplay);

                gameplay.getBoard().setGamePlay(gameplay);
        }

        MultiPlayerSwingView gui = new MultiPlayerSwingView(this, games,
                        this._config, true);

        for(int i = 0; i < this._config.getNbPlayersMulti(); i++)
                gui.addControllerPlayer(i, new LocalController(games.get(i)));

        gui.run();
    }

    private void jButtonOptionsActionPerformed(ActionEvent evt)
    {
        OptionsView ov = new OptionsView(this, this._config);
        this.setVisible(false);
        ov.setVisible(true);
    }
}

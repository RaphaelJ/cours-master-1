package view;

import game.Game;
import game.multi.MultiClassic;
import game.multi.MultiCooperative;
import game.multi.MultiGame;
import game.rules.NintendoGameBoy;
import game.rules.NintendoGameBoyFactory;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

import model.Board;
import model.config.LocalConfig;
import model.config.OnlineConfig;
import util.random.LCGRandom;
import view.online.JoinServerView;
import view.online.ServerOptionsView;

public class WelcomeView extends javax.swing.JFrame {

	private static final long serialVersionUID = -2260957186452467297L;

	private JPanel jPanelButtons;

    private JButton jButtonCoop;
    private JButton jButtonMultiClassic;
    private JButton jButtonMultiSimple;
    private JButton jButtonJoinServer;
    private JButton jButtonStartServer;
    private JButton jButtonOptions;
    private JButton jButtonSolo;
    private JButton jButtonExit;

    private LocalConfig _config;

    public WelcomeView()
    {
        super("Tetris MVC");

        this._config = new LocalConfig();

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
        jButtonStartServer = new JButton();
        jButtonJoinServer = new JButton();
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

        jButtonJoinServer.setText("Join a server");
        jButtonJoinServer.setAlignmentX(CENTER_ALIGNMENT);
        jButtonJoinServer.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jButtonJoinServerActionPerformed(evt);
            }
        });

        jButtonStartServer.setText("Start a server");
        jButtonStartServer.setAlignmentX(CENTER_ALIGNMENT);
        jButtonStartServer.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jButtonStartServerActionPerformed(evt);
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

        GridLayout layout = new GridLayout(8, 1);
        layout.setVgap(10);
        jPanelButtons.setLayout(layout);
        jPanelButtons.add(jButtonSolo);
        jPanelButtons.add(jButtonMultiSimple);
        jPanelButtons.add(jButtonMultiClassic);
        jPanelButtons.add(jButtonCoop);
        jPanelButtons.add(jButtonJoinServer);
        jPanelButtons.add(jButtonStartServer);
        jPanelButtons.add(jButtonOptions);
        jPanelButtons.add(jButtonExit);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(10, 10, 10, 10);

        this.getContentPane().add(jPanelButtons, gbc);

        pack();
    }

    private void jButtonSoloActionPerformed(ActionEvent evt)
    {
        this.setVisible(false);

        Board board = new Board(
            this._config.getBoardWidth(), this._config.getBoardHeight()
        );
        Game game = new Game(board, new NintendoGameBoy());

        new SinglePlayerSwingView(this, game, this._config).run();
    }

    private void jButtonMultiSimpleActionPerformed(ActionEvent evt)
    {
        ArrayList<Board> boards = new ArrayList<Board>();
        LCGRandom seedGenerator = new LCGRandom();

        for(int i = 0; i < this._config.getNbPlayersMulti(); i++) {
            boards.add(
                new Board(
                    new LCGRandom(seedGenerator.nextInt()),
                    this._config.getBoardWidth(),this._config.getBoardHeight()
                )
            );
        }

        startMultiPlayersGame(
            new MultiGame(boards, new NintendoGameBoyFactory())
        );
    }

    private void jButtonMultiClassicActionPerformed(ActionEvent evt)
    {
        ArrayList<Board> boards = new ArrayList<Board>();
        long commonSeed = new LCGRandom().getSeed();

        for(int i = 0; i < this._config.getNbPlayersMulti(); i++) {
            boards.add(
                new Board(
                    new LCGRandom(commonSeed), this._config.getBoardWidth(),
                    this._config.getBoardHeight()
                )
            );
        }

        int posHole = new LCGRandom().nextInt(this._config.getBoardWidth());

        startMultiPlayersGame(
            new MultiClassic(boards, new NintendoGameBoyFactory(), posHole)
        );
    }

    private void jButtonCoopActionPerformed(ActionEvent evt)
    {
        ArrayList<Board> boards = new ArrayList<Board>();
        LCGRandom seedGenerator = new LCGRandom();

        for(int i = 0; i < this._config.getNbPlayersMulti(); i++) {
            boards.add(
                new Board(
                    new LCGRandom(seedGenerator.nextInt()),
                    this._config.getBoardWidth(),this._config.getBoardHeight()
                )
            );
        }

        startMultiPlayersGame(
            new MultiCooperative(boards, new NintendoGameBoyFactory())
        );
    }

    private void jButtonJoinServerActionPerformed(ActionEvent evt)
    {
        JoinServerView jsv = new JoinServerView();
        this.setVisible(false);
        jsv.setVisible(true);
    }

    private void jButtonStartServerActionPerformed(ActionEvent evt)
    {
        OnlineConfig oc = new OnlineConfig(this._config);
        ServerOptionsView sov = new ServerOptionsView(oc);
        this.setVisible(false);
        sov.setVisible(true);
    }

    private void startMultiPlayersGame(MultiGame game)
    {
        this.setVisible(false);

        new MultiPlayerSwingView(this, game, this._config, true).run();
    }

    private void jButtonOptionsActionPerformed(ActionEvent evt)
    {
        OptionsView ov = new OptionsView(this, this._config);
        this.setVisible(false);
        ov.setVisible(true);
    }
}

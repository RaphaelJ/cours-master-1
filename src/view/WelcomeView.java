package view;

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

import game.*;
import game.multi.*;
import game.rules.*;
import model.Board;
import model.config.LocalConfig;
import model.config.OnlineConfig;
import util.random.LCGRandom;
import view.online.JoinServerView;
import view.online.ServerOptionsView;

public class WelcomeView extends javax.swing.JFrame {

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

        new SinglePlayerSwingView(
            this, new Game(
                this._config.getBoardWidth(), this._config.getBoardHeight(),
                new NintendoGameBoy()
            ), this._config
        ).run();
    }

    private void jButtonMultiSimpleActionPerformed(ActionEvent evt)
    {
        startMultiPlayersGame(
            new MultiGame(
                this._config.getBoardWidth(), this._config.getBoardHeight(),
                this._config.getNbPlayersMulti(), new NintendoGameBoyFactory()
            )
        );
    }

    private void jButtonMultiClassicActionPerformed(ActionEvent evt)
    {
        int posHole = new LCGRandom().nextInt(this._config.getBoardWidth());

        startMultiPlayersGame(
            new MultiClassic(
                this._config.getBoardWidth(), this._config.getBoardHeight(),
                this._config.getNbPlayersMulti(), new NintendoGameBoyFactory(),
                posHole
            )
        );
    }

    private void jButtonCoopActionPerformed(ActionEvent evt)
    {
        startMultiPlayersGame(
            new MultiCooperative(
                this._config.getBoardWidth(), this._config.getBoardHeight(),
                this._config.getNbPlayersMulti(), new NintendoGameBoyFactory()
            )
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

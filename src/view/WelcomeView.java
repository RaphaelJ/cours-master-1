package view;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import controller.LocalController;
import gameplay.GamePlay;
import gameplay.GamePlayFactory;
import gameplay.NintendoGameBoy;
import gameplay.NintendoGameBoyFactory;
import gameplay.multi.DualClassic;
import gameplay.multi.DualCooperative;
import gameplay.multi.DualGamePlay;

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
	
    public WelcomeView()
    {
    	super("Tetris MVC");
    	
        initComponents();
    }

    private void initComponents() {

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

        Board board = new Board();
        GamePlay game = new NintendoGameBoy(board);

        board.setGamePlay(game);

        SinglePlayerSwingView gui = new SinglePlayerSwingView(this, game, true);

        gui.addController(new LocalController(game));

        gui.run();
    }

    private void jButtonMultiSimpleActionPerformed(ActionEvent evt) {
        Board board1 = new Board(new LCGRandom()),
              board2 = new Board(new LCGRandom());

        GamePlayFactory innerGameplay = new NintendoGameBoyFactory();
        DualGamePlay game = new DualGamePlay(innerGameplay, board1, board2);

        startTwoPlayersGame(game);
    }

    private void jButtonMultiClassicActionPerformed(ActionEvent evt) {
        long commonSeed = new LCGRandom().getSeed();

        Board board1 = new Board(new LCGRandom(commonSeed)),
              board2 = new Board(new LCGRandom(commonSeed));

        GamePlayFactory innerGameplay = new NintendoGameBoyFactory();
        
        int posHole = new LCGRandom().nextInt(board1.getWidth());
        DualGamePlay game = new DualClassic(
        		innerGameplay,
        		board1,
        		board2,
        		posHole);

        startTwoPlayersGame(game);
    }

    private void jButtonCoopActionPerformed(ActionEvent evt) {
        Board board1 = new Board(), board2 = new Board();

        GamePlayFactory innerGameplay = new NintendoGameBoyFactory();
        DualCooperative game = new DualCooperative(
            innerGameplay, board1, board2
        );

        startTwoPlayersGame(game);
    }

    private void startTwoPlayersGame(DualGamePlay game)
    {
        this.setVisible(false);

        GamePlay game1 = game.getPlayer1GamePlay(),
                 game2 = game.getPlayer2GamePlay();

        game1.getBoard().setGamePlay(game1);
        game2.getBoard().setGamePlay(game2);

        TwoPlayersSwingView gui = new TwoPlayersSwingView(this, game1, game2, true);

        gui.addControllerPlayer1(new LocalController(game1));
        gui.addControllerPlayer2(new LocalController(game2));

        gui.run();
    }
    
    private void jButtonOptionsActionPerformed(ActionEvent evt) {
        Board board1 = new Board(), board2 = new Board();

        GamePlayFactory innerGameplay = new NintendoGameBoyFactory();
        DualCooperative game = new DualCooperative(
            innerGameplay, board1, board2
        );

        startTwoPlayersGame(game);
    }

}

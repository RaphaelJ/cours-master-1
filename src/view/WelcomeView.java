package view;

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

import javax.swing.GroupLayout;
import javax.swing.JButton;
import javax.swing.LayoutStyle;
import javax.swing.WindowConstants;

import model.Board;
import util.random.LCGRandom;

public class WelcomeView extends javax.swing.JFrame {

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

        jButtonSolo = new JButton();
        jButtonMultiSimple = new JButton();
        jButtonMultiClassic = new JButton();
        jButtonCoop = new JButton();
        jButtonOptions = new JButton();
        jButtonExit = new JButton();
        
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);

        jButtonSolo.setText("Solo mode");
        jButtonSolo.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jButtonSoloActionPerformed(evt);
            }
        });

        jButtonMultiSimple.setText("Multi simple mode");
        jButtonMultiSimple.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jButtonMultiSimpleActionPerformed(evt);
            }
        });

        jButtonMultiClassic.setText("Multi classic mode");
        jButtonMultiClassic.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jButtonMultiClassicActionPerformed(evt);
            }
        });

        jButtonCoop.setText("Cooperative mode");
        jButtonCoop.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                jButtonCoopActionPerformed(evt);
            }
        });

        jButtonOptions.setText("Options");
        
        jButtonExit.setText("Exit");
        jButtonExit.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
					setVisible(false);
					System.exit(0);
			}
		});

        GroupLayout layout = new GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(GroupLayout.Alignment.CENTER)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(
                		GroupLayout.Alignment.CENTER)
                    .addComponent(jButtonMultiSimple, GroupLayout.DEFAULT_SIZE,
                    		GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jButtonSolo, GroupLayout.DEFAULT_SIZE,
                    		GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jButtonMultiClassic, GroupLayout.DEFAULT_SIZE,
                    		GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jButtonCoop, GroupLayout.DEFAULT_SIZE,
                    		GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
/*                     .addComponent(jButtonOptions, GroupLayout.DEFAULT_SIZE,
                    		GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)*/
                    .addComponent(jButtonExit, GroupLayout.DEFAULT_SIZE,
                    		GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    )
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(GroupLayout.Alignment.CENTER)
            .addGroup(GroupLayout.Alignment.CENTER,
            		layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jButtonSolo)
                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonMultiSimple)
                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonMultiClassic)
                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonCoop)
                .addGap(18, 18, 18)
//                 .addComponent(jButtonOptions)
                .addComponent(jButtonExit)
                .addContainerGap())
        );

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
        //this.setVisible(true);
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
        //this.setVisible(true);

    }
}

package view;

import controller.LocalController;
import gameplay.GamePlay;
import gameplay.GamePlayFactory;
import gameplay.NintendoGameBoy;
import gameplay.NintendoGameBoyFactory;
import gameplay.multi.DualClassic;
import gameplay.multi.DualCooperative;
import gameplay.multi.DualGamePlay;

import java.util.ArrayList;

import javax.swing.GroupLayout;
import javax.swing.LayoutStyle;

import model.Board;
import util.random.LCGRandom;

public class WelcomeView extends javax.swing.JFrame {

    public WelcomeView()
    {
        initComponents();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jButtonSolo = new javax.swing.JButton();
        jButtonMultiSimple = new javax.swing.JButton();
        jButtonMultiClassic = new javax.swing.JButton();
        jButtonCoop = new javax.swing.JButton();
        jButtonOptions = new javax.swing.JButton();
        jLabelTitle = new javax.swing.JLabel();

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);

        jButtonSolo.setText("Solo mode");
        jButtonSolo.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonSoloActionPerformed(evt);
            }
        });

        jButtonMultiSimple.setText("Multi simple mode");
        jButtonMultiSimple.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonMultiSimpleActionPerformed(evt);
            }
        });

        jButtonMultiClassic.setText("Multi classic mode");
        jButtonMultiClassic.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonMultiClassicActionPerformed(evt);
            }
        });

        jButtonCoop.setText("Cooperative mode");
        jButtonCoop.setToolTipText("");
        jButtonCoop.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonCoopActionPerformed(evt);
            }
        });

        jButtonOptions.setText("Options");

        jLabelTitle.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        jLabelTitle.setText("Tetris");

        GroupLayout layout = new GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
                    .addComponent(jButtonMultiSimple, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jButtonSolo, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jButtonMultiClassic, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jButtonCoop, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
//                     .addComponent(jButtonOptions, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    )
                .addContainerGap())
            .addGroup(layout.createSequentialGroup()
                .addGap(71, 71, 71)
                .addComponent(jLabelTitle, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGap(75, 75, 75))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(GroupLayout.Alignment.LEADING)
            .addGroup(GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addGap(27, 27, 27)
                .addComponent(jLabelTitle, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGap(32, 32, 32)
                .addComponent(jButtonSolo)
                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonMultiSimple)
                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonMultiClassic)
                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonCoop)
                .addGap(18, 18, 18)
//                 .addComponent(jButtonOptions)
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void jButtonSoloActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonSoloActionPerformed
        this.setVisible(false);

        Board board = new Board();
        GamePlay game = new NintendoGameBoy(board);

        board.setGamePlay(game);

        SinglePlayerSwingView gui = new SinglePlayerSwingView(game, true);

        gui.addController(new LocalController(game));

        gui.run();
    }//GEN-LAST:event_jButtonSoloActionPerformed

    private void jButtonMultiSimpleActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonMultiSimpleActionPerformed
        long commonSeed = new LCGRandom().getSeed();

        Board board1 = new Board(new LCGRandom(commonSeed)),
              board2 = new Board(new LCGRandom(commonSeed));

        GamePlayFactory innerGameplay = new NintendoGameBoyFactory();
        DualGamePlay game = new DualGamePlay(innerGameplay, board1, board2);

        startTwoPlayersGame(game);
    }//GEN-LAST:event_jButtonMultiSimpleActionPerformed

    private void jButtonMultiClassicActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonMultiClassicActionPerformed
        long commonSeed = new LCGRandom().getSeed();

        Board board1 = new Board(new LCGRandom(commonSeed)),
              board2 = new Board(new LCGRandom(commonSeed));

        GamePlayFactory innerGameplay = new NintendoGameBoyFactory();
        DualGamePlay game = new DualClassic(innerGameplay, board1, board2);

        startTwoPlayersGame(game);
    }//GEN-LAST:event_jButtonMultiClassicActionPerformed

    private void jButtonCoopActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonCoopActionPerformed
        Board board1 = new Board(), board2 = new Board();

        GamePlayFactory innerGameplay = new NintendoGameBoyFactory();
        DualCooperative game = new DualCooperative(
            innerGameplay, board1, board2
        );

        startTwoPlayersGame(game);
    }//GEN-LAST:event_jButtonCoopActionPerformed

    private void startTwoPlayersGame(DualGamePlay game)
    {
        this.setVisible(false);

        GamePlay game1 = game.getPlayer1GamePlay(),
                 game2 = game.getPlayer2GamePlay();

        game1.getBoard().setGamePlay(game1);
        game2.getBoard().setGamePlay(game2);

        TwoPlayersSwingView gui = new TwoPlayersSwingView(game1, game2, true);

        gui.addControllerPlayer1(new LocalController(game1));
        gui.addControllerPlayer2(new LocalController(game2));

        gui.run();

    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonCoop;
    private javax.swing.JButton jButtonMultiClassic;
    private javax.swing.JButton jButtonMultiSimple;
    private javax.swing.JButton jButtonOptions;
    private javax.swing.JButton jButtonSolo;
    private javax.swing.JLabel jLabelTitle;
    // End of variables declaration//GEN-END:variables
}

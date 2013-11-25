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

    private ArrayList<GamePlay> _gameplays = new ArrayList<GamePlay>();
    
    public WelcomeView() {
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
                    .addComponent(jButtonOptions, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
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
                .addComponent(jButtonOptions)
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void jButtonSoloActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonSoloActionPerformed
        Board board = new Board();
        GamePlay gameplay = new NintendoGameBoy(board);
        
        board.setGamePlay(gameplay);
        this._gameplays.add(gameplay);
        
        startGame();
    }//GEN-LAST:event_jButtonSoloActionPerformed

    private void jButtonMultiSimpleActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonMultiSimpleActionPerformed
        long commonSeed = new LCGRandom().getSeed();
        
        Board boardP1 = new Board(new LCGRandom(commonSeed)),
              boardP2 = new Board(new LCGRandom(commonSeed));
        
        GamePlayFactory innerGameplay = new NintendoGameBoyFactory();
        DualGamePlay gameplay = new DualGamePlay(innerGameplay, boardP1, boardP2);
        setDualGameplays(gameplay, boardP1, boardP2);
        
        startGame();
    }//GEN-LAST:event_jButtonMultiSimpleActionPerformed

    private void jButtonMultiClassicActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonMultiClassicActionPerformed
        long commonSeed = new LCGRandom().getSeed();
        
        Board boardP1 = new Board(new LCGRandom(commonSeed)),
              boardP2 = new Board(new LCGRandom(commonSeed));
        
        GamePlayFactory innerGameplay = new NintendoGameBoyFactory();
        DualClassic gameplay = new DualClassic(innerGameplay, boardP1, boardP2);
        setDualGameplays(gameplay, boardP1, boardP2);
        
        startGame();
    }//GEN-LAST:event_jButtonMultiClassicActionPerformed

    private void jButtonCoopActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonCoopActionPerformed
        long commonSeed = new LCGRandom().getSeed();
        
        Board boardP1 = new Board(new LCGRandom(commonSeed)),
              boardP2 = new Board(new LCGRandom(commonSeed));
        
        GamePlayFactory innerGameplay = new NintendoGameBoyFactory();
        DualCooperative gameplay = new DualCooperative(innerGameplay, boardP1, boardP2);
        setDualGameplays(gameplay, boardP1, boardP2);
        
        startGame();
    }//GEN-LAST:event_jButtonCoopActionPerformed

    private void setDualGameplays(DualGamePlay gameplay, Board boardP1,
            Board boardP2) {
        GamePlay gameplayP1 = gameplay.getPlayer1GamePlay(),
                 gameplayP2 = gameplay.getPlayer2GamePlay();
        
        boardP1.setGamePlay(gameplayP1);
        boardP2.setGamePlay(gameplayP2);
        
        this._gameplays.add(gameplayP1);
        this._gameplays.add(gameplayP2);
        
        startGame();
    }
    
    public ArrayList<GamePlay> getGamePlays() {
        return this._gameplays;
    }
    
    private void startGame() {
        
        this.setVisible(false);
        
        if(this._gameplays.isEmpty())
            System.exit(0);

        SwingView gui = new SwingView(this._gameplays, true);

        // Controller which listen to GUI events and transmits them to the
        // game's manager.
        for(GamePlay gameplay : this._gameplays)
            gui.addController(new LocalController(gameplay));

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

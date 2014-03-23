package view.online;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JList;

public class LobbyView extends JFrame {

    private JList _playerList;
    private JButton _launchButton;

    public LobbyView()
    {
        initComponents();
    }

    private void initComponents()
    {
        this._playerList = new JList();
        this._launchButton = new JButton("Launch");
        this._launchButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                    launchGame();
            }
        });
    }

    private void launchGame()
    {

    }
}

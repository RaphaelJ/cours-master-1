package view.online;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;

public class ServerStatusView extends JFrame {

    private JPanel _panel;
    
    private JLabel _logLabel;
    private JTextArea _logTextArea;

    public ServerStatusView() {
        super("Server status");
        
        initComponents();
    }
    
    private void initComponents() {
        
        // Setup the components
        this._panel = new JPanel();
        this._panel.setLayout(new BoxLayout(this._panel, BoxLayout.Y_AXIS));
        
        this._logLabel = new JLabel("Log : ");
        this._logTextArea = new JTextArea();
        this._logTextArea.setEditable(false);
        
        // Add the components to the panel
        this._panel.add(this._logLabel);
        this._panel.add(this._logTextArea);
        
        // Setup the frame
        this.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
        this.add(this._panel);
        this.setSize(500, 300);
    }
    
    public void log(String message) {
        this._logTextArea.append(message);
    }
    
    public static void main(String[] args) {
        ServerStatusView serverStatusView = new ServerStatusView();
        serverStatusView.setVisible(true);
    }
}

package view;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

/** Dialog from the option menu which can be used by the user to modify the
 * given KeySet. */
public class KeySetDialog extends JDialog
            implements FocusListener, KeyListener {

    private KeySet _keySet;

    private JPanel _formPanel;
    private JPanel _buttonsPanel;

    private JLabel _rotateLabel;
    private JLabel _leftLabel;
    private JLabel _rightLabel;
    private JLabel _softDropLabel;
    private JLabel _hardDropLabel;

    private JTextField _rotateTextField;
    private JTextField _leftTextField;
    private JTextField _rightTextField;
    private JTextField _softDropTextField;
    private JTextField _hardDropTextField;

    private JButton _okButton;

    public KeySetDialog(Frame owner, boolean modal, KeySet keySet)
    {
        super(owner, "Key set editor", modal);

        this._keySet = keySet;

        initComponents();
    }

    private void initComponents()
    {
        // Initialize the panels
        this._formPanel = new JPanel(new GridBagLayout());
        this._buttonsPanel = new JPanel(new FlowLayout());

        // Initialize the labels
        this._rotateLabel = new JLabel("Rotate key : ");
        this._leftLabel = new JLabel("Left key : ");
        this._rightLabel = new JLabel("Right key : ");
        this._softDropLabel = new JLabel("Soft drop key : ");
        this._hardDropLabel = new JLabel("Hard drop key : ");

        // Initialize the text fields
        this._rotateTextField = new JTextField(
                        KeyEvent.getKeyText(this._keySet.getKeyRotate()));
        this._rotateTextField.setName("rotate");
        this._rotateTextField.setEditable(false);
        this._rotateTextField.addFocusListener(this);
        this._rotateTextField.addKeyListener(this);

        this._leftTextField = new JTextField(
                        KeyEvent.getKeyText(this._keySet.getKeyLeft()));
        this._leftTextField.setName("left");
        this._leftTextField.setEditable(false);
        this._leftTextField.addFocusListener(this);
        this._leftTextField.addKeyListener(this);

        this._rightTextField = new JTextField(
                        KeyEvent.getKeyText(this._keySet.getKeyRight()));
        this._rightTextField.setName("right");
        this._rightTextField.setEditable(false);
        this._rightTextField.addFocusListener(this);
        this._rightTextField.addKeyListener(this);

        this._softDropTextField = new JTextField(
                        KeyEvent.getKeyText(this._keySet.getKeySoftDrop()));
        this._softDropTextField.setName("softDrop");
        this._softDropTextField.setEditable(false);
        this._softDropTextField.addFocusListener(this);
        this._softDropTextField.addKeyListener(this);

        this._hardDropTextField = new JTextField(
                        KeyEvent.getKeyText(this._keySet.getKeyHardDrop()));
        this._hardDropTextField.setName("hardDrop");
        this._hardDropTextField.setEditable(false);
        this._hardDropTextField.addFocusListener(this);
        this._hardDropTextField.addKeyListener(this);

        // Initialize the buttons
        this._okButton = new JButton("OK");
        this._okButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setVisible(false);
                dispose();
            }
        });

        // Add components to the formPanel
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.EAST;
        int i = 0;

        gbc.gridx = 0;
        gbc.gridy = i;
        gbc.weightx = 0.75;
        gbc.fill = GridBagConstraints.NONE;
        this._formPanel.add(this._rotateLabel, gbc);

        gbc.gridx = 1;
        gbc.gridy = i;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        this._formPanel.add(this._rotateTextField, gbc);
        i++;

        gbc.gridx = 0;
        gbc.gridy = i;
        gbc.weightx = 0.75;
        gbc.fill = GridBagConstraints.NONE;
        this._formPanel.add(this._leftLabel, gbc);

        gbc.gridx = 1;
        gbc.gridy = i;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        this._formPanel.add(this._leftTextField, gbc);
        i++;

        gbc.gridx = 0;
        gbc.gridy = i;
        gbc.weightx = 0.75;
        gbc.fill = GridBagConstraints.NONE;
        this._formPanel.add(this._rightLabel, gbc);

        gbc.gridx = 1;
        gbc.gridy = i;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        this._formPanel.add(this._rightTextField, gbc);
        i++;

        gbc.gridx = 0;
        gbc.gridy = i;
        gbc.weightx = 0.75;
        gbc.fill = GridBagConstraints.NONE;
        this._formPanel.add(this._softDropLabel, gbc);

        gbc.gridx = 1;
        gbc.gridy = i;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        this._formPanel.add(this._softDropTextField, gbc);
        i++;

        gbc.gridx = 0;
        gbc.gridy = i;
        gbc.weightx = 0.75;
        gbc.fill = GridBagConstraints.NONE;
        this._formPanel.add(this._hardDropLabel, gbc);

        gbc.gridx = 1;
        gbc.gridy = i;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        this._formPanel.add(this._hardDropTextField, gbc);

        // Add buttons to buttonsPanel
        this._buttonsPanel.add(this._okButton);

        // Setting up the dialog
        this.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
        this.setLayout(new BorderLayout(6, 6));

        // Add panels to the dialog
        this.add(this._formPanel, BorderLayout.NORTH);
        this.add(this._buttonsPanel, BorderLayout.SOUTH);

        this.pack();
    }

    public static void main(String[] args)
    {
        KeySetDialog ksd = new KeySetDialog(null, false, KeySet.getKeySet(0));
        ksd.setVisible(true);
    }

    @Override
    public void focusGained(FocusEvent e) {
        Object source  = e.getSource();

        if(source instanceof JTextField) {
            JTextField textField = (JTextField) source;
            textField.setText("");
        }
    }

    @Override
    public void focusLost(FocusEvent e)
    {
        Object source  = e.getSource();

        if(source instanceof JTextField) {
            JTextField textField = (JTextField) source;

            switch(textField.getName()) {
            case "rotate":
                this._rotateTextField.setText(
                    KeyEvent.getKeyText(this._keySet.getKeyRotate()));
                break;
            case "left":
                this._leftTextField.setText(
                    KeyEvent.getKeyText(this._keySet.getKeyLeft()));
                break;
            case "right":
                this._rightTextField.setText(
                    KeyEvent.getKeyText(this._keySet.getKeyRight()));
                break;
            case "softDrop":
                this._softDropTextField.setText(
                    KeyEvent.getKeyText(this._keySet.getKeySoftDrop()));
                break;
            case "hardDrop":
                this._hardDropTextField.setText(
                    KeyEvent.getKeyText(this._keySet.getKeyHardDrop()));
                break;
            }
        }
    }

    @Override
    public void keyPressed(KeyEvent e)
    {
        /* Can't assign the key used to pause the game or a key that is
            * already assigned.
            */
        if(e.getKeyCode() == KeyEvent.VK_P
           || this._keySet.getKeyList().contains(e.getKeyCode()))
                return;

        Object source  = e.getSource();

        if(source instanceof JTextField) {
            JTextField textField = (JTextField) source;

            switch(textField.getName()) {
                case "rotate":
                    this._keySet.setKeyRotate(e.getKeyCode());
                    this._rotateTextField.setText(
                        KeyEvent.getKeyText(this._keySet.getKeyRotate()));
                    break;
                case "left":
                    this._keySet.setKeyLeft(e.getKeyCode());
                    this._leftTextField.setText(
                        KeyEvent.getKeyText(this._keySet.getKeyLeft()));
                    break;
                case "right":
                    this._keySet.setKeyRight(e.getKeyCode());
                    this._rightTextField.setText(
                        KeyEvent.getKeyText(this._keySet.getKeyRight()));
                    break;
                case "softDrop":
                    this._keySet.setKeySoftDrop(e.getKeyCode());
                    this._softDropTextField.setText(
                        KeyEvent.getKeyText(this._keySet.getKeySoftDrop()));
                    break;
                case "hardDrop":
                    this._keySet.setKeyHardDrop(e.getKeyCode());
                    this._hardDropTextField.setText(
                        KeyEvent.getKeyText(this._keySet.getKeyHardDrop()));
                    break;
            }

            _rotateLabel.requestFocusInWindow();
        }
    }

    @Override
    public void keyReleased(KeyEvent e) { }

    @Override
    public void keyTyped(KeyEvent e) { }
}

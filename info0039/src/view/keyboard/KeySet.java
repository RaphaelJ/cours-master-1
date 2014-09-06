package view.keyboard;

import java.awt.event.KeyEvent;
import java.util.ArrayList;

/** Contains the key mapping for a player. */
public class KeySet {

    private int keyRotate;
    private int keyRight;
    private int keyLeft;
    private int keySoftDrop;
    private int keyHardDrop;

    public KeySet(int rotate, int left, int right, int softDrop, int hardDrop)
    {
        this.keyRotate = rotate;
        this.keyRight = right;
        this.keyLeft = left;
        this.keySoftDrop = softDrop;
        this.keyHardDrop = hardDrop;
    }

    public int getKeyRotate()
    {
        return keyRotate;
    }

    public int getKeyRight()
    {
        return keyRight;
    }

    public int getKeyLeft()
    {
        return keyLeft;
    }

    public int getKeySoftDrop()
    {
        return keySoftDrop;
    }

    public int getKeyHardDrop()
    {
        return keyHardDrop;
    }

    public void setKeyRotate(int keyUp)
    {
        this.keyRotate = keyUp;
    }

    public void setKeyRight(int keyRight)
    {
        this.keyRight = keyRight;
    }

    public void setKeyLeft(int keyLeft)
    {
        this.keyLeft = keyLeft;
    }

    public void setKeySoftDrop(int keyDown)
    {
        this.keySoftDrop = keyDown;
    }

    public void setKeyHardDrop(int keyHardDrop)
    {
        this.keyHardDrop = keyHardDrop;
    }

    public ArrayList<Integer> getKeyList()
    {
        ArrayList<Integer> keyList = new  ArrayList<Integer>();

        keyList.add(this.keyRotate);
        keyList.add(this.keyLeft);
        keyList.add(this.keyRight);
        keyList.add(this.keySoftDrop);
        keyList.add(this.keyHardDrop);

        return keyList;
    }

    public static KeySet getKeySet(int numPlayer)
    {
        switch(numPlayer) {
            case 0:
                return new KeySet(  KeyEvent.VK_UP,
                                    KeyEvent.VK_LEFT,
                                    KeyEvent.VK_RIGHT,
                                    KeyEvent.VK_DOWN,
                                    KeyEvent.VK_ENTER);
            case 1:
                return new KeySet(  KeyEvent.VK_Z,
                                    KeyEvent.VK_Q,
                                    KeyEvent.VK_D,
                                    KeyEvent.VK_S,
                                    KeyEvent.VK_A);
            case 2:
                return new KeySet(  KeyEvent.VK_T,
                                    KeyEvent.VK_F,
                                    KeyEvent.VK_H,
                                    KeyEvent.VK_G,
                                    KeyEvent.VK_R);
            case 3:
                return new KeySet(  KeyEvent.VK_O,
                                    KeyEvent.VK_K,
                                    KeyEvent.VK_M,
                                    KeyEvent.VK_L,
                                    KeyEvent.VK_I);
            default:
                return null;
        }
    }
}

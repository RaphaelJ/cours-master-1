package util;

import java.util.Date;
import java.util.Timer;
import java.util.TimerTask;

/** Provides a timer which controll the game. */
public class GameTimer {

    private Timer _timer = null;
    private Runnable _task;
    private int _clockDelay;

    // Computes the duration of the game by taking into account pauses with two
    // variables. The first contains the number of milliseconds which have been
    // elapsed before the last pause (0 when no previous pauses). The second
    // contains the ending date of the last pause.
    private long _elapsed = 0;
    private Date _lastStart;

    public GameTimer(Runnable task, int clockDelay)
    {
        this._task = task;
        this._clockDelay = clockDelay;
    }

    public boolean isRunning()
    {
        return this._timer != null;
    }

    public synchronized void start()
    {
        if (!this.isRunning()) {
            this._timer = new Timer();
            this._timer.scheduleAtFixedRate(
                new TimerTask() {
                    @Override
                    public void run()
                    {
                        _task.run();
                    }
                }, this._clockDelay, this._clockDelay
            );
            this._lastStart = new Date();
        }
    }

    public synchronized void stop()
    {
        if (this.isRunning()) {
            this._elapsed = this.getElapsedTime();
            this._timer.cancel();
            this._timer = null;
        }
    }

    public synchronized void changeSpeed(int newClockDelay)
    {
        this._clockDelay = newClockDelay;

        if (this.isRunning()) {
            this.stop();
            this.start();
        }
    }

    /** Returns the total gaming time without pauses in milliseconds. */
    public synchronized long getElapsedTime()
    {
        if (!this.isRunning())
            return this._elapsed;
        else {
            long sinceStart = System.currentTimeMillis() - _lastStart.getTime();
            return this._elapsed + sinceStart;
        }
    }
}

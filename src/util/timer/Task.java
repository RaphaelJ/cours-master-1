package util.timer;

public abstract class Task implements Runnable {
	
	protected long delay;
	protected long expireTime;
	
	protected boolean iterative;
	protected boolean cancelled;
	
	public Task(boolean iterative) {
		this.iterative = iterative;
	}
	
	public boolean isCancelled() {
		return cancelled;
	}
	
	public void cancel() {
		this.cancelled = true;
	}
	
	public long getExpireTime() {
		return expireTime;
	}
	
	public void schedule(long delay) {
		this.delay = delay;
		this.expireTime = System.currentTimeMillis() + delay;
	}
	
	public void reschedule() {
		if(iterative)
			this.expireTime = System.currentTimeMillis() + delay;
		else
			cancel();
	}
	
	public abstract void run();
}

package model.timer;

public class Timer {

	private TimerThread innerThread;
	private Task timerTask;
	
	public Timer(Task task, long delay) {
		
		this.timerTask = task;
		this.timerTask.schedule(delay);
		
		this.innerThread = new TimerThread();
		this.innerThread.start();
	}
	
	public void cancel() {
		innerThread.halt();
	}
	
	public class TimerThread extends Thread {
		
		private boolean stopped = false;

		private void halt() {
			stopped = true;
		}
		
		@Override
		public void run() {
			
			Task taskToRun = null;
			
			while(!stopped) {
				
				if(taskToRun != null) {
					
					if(taskToRun.isCancelled()) {
						taskToRun = null;
						halt();
						continue;
					}
					
					taskToRun.run();
					taskToRun.reschedule();
					taskToRun = null;
				}
				
				long timeDelta = timerTask.getExpireTime() - System.currentTimeMillis();
				
				if(timeDelta <= 0)
					taskToRun = timerTask;
			}
		}
	}
}

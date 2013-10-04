package util.timer;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class TaskImpl extends Task {

	private Object target;
	private String methodName;
	private Class<?>[] parameterTypes;
	private Object[] parameters;
	
	public TaskImpl(boolean iterative, Object target, String methodName,
			Class<?>[] parameterTypes, Object[] parameters) {
		super(iterative);
		
		this.target = target;
		this.methodName = methodName;
		this.parameterTypes = parameterTypes;
		this.parameters = parameters;
	}
	
	@Override
	public void run() {
		
		try {
			Method method = target.getClass().getMethod(methodName, parameterTypes);
			method.invoke(target, parameters);
			
		} catch (IllegalArgumentException e) {
			e.printStackTrace();
		} catch (SecurityException e) {
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			e.printStackTrace();
		}
	}
}

public class Trampoline {
    public static final int stackDepth = 100;

    public Object[] call(Closure closure, Object[] args) {
	while (true) {
	    try {
		if (closure != null) {
		    closure.apply(stackDepth, args);
		}
	    } catch (StackExhausted se) {
		closure = se.closure;
		args = se.args;
		continue;
	    }
	    break;
	}
	return args;
    }
}

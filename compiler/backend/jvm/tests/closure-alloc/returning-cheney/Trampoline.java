public class Trampoline {
    public static final int stackDepth = 10;

    public Closure closure;
    public Object[] args;

    public Object[] call(Closure closure, Object[] args) {
	this.closure = closure;
	this.args = args;
	while (this.closure != null) {
	    this.closure.apply(stackDepth, this.args, this);
	}
	return this.args;
    }
}

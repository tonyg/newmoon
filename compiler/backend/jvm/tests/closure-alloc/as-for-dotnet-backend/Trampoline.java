public class Trampoline {
    public Closure closure;
    public Object[] args;

    public Object[] call(Closure closure, Object[] args) {
	this.closure = closure;
	this.args = args;
	while (this.closure != null) {
	    this.closure.apply(this.args, this);
	}
	return this.args;
    }
}

public class FinalK extends Closure {
    public void apply(int stackLeft, Object[] args, Trampoline next) {
	next.closure = null;
	next.args = args;
    }
}
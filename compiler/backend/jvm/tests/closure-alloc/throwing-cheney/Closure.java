public abstract class Closure {
    public abstract void apply(int stackLeft, Object[] args)
	throws StackExhausted;
}

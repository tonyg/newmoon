public class C1 extends Closure {
    public static String fileName = "SomeClosure.scheme";
    public static int lineNumber = 1234;

    public Object env_1;
    public Object env_2;
    public Object env_3;
    public Object env_4;

    public void apply(int stackLeft, Object[] args)
	throws StackExhausted
    {
	if (stackLeft == 0) {
	    StackExhausted se = new StackExhausted();
	    se.closure = this;
	    se.args = args;
	    throw se;
	}

	long counter = ((Long)args[0]).longValue();
	if (counter == 0) {
	    StackExhausted se = new StackExhausted();
	    se.closure = null;
	    se.args = new Object[] { env_4 };
	    throw se;
	} else {
	    C1 c = new C1();
	    c.env_1 = env_2;
	    c.env_2 = env_3;
	    c.env_3 = env_4;
	    c.env_4 = args[0];
	    c.apply(stackLeft - 1, new Object[] { new Long(counter - 1) });
	    return;
	}
    }
}

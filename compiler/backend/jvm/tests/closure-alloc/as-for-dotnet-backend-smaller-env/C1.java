public class C1 extends Closure {
    public static String fileName = "SomeClosure.scheme";
    public static int lineNumber = 1234;

    public Object env_1;

    public void apply(Object[] args, Trampoline next) {
	long counter = ((Long)args[0]).longValue();
	if (counter == 0) {
	    next.closure = null;
	    next.args = new Object[] { env_1 };
	    return;
	} else {
	    C1 c = new C1();
	    c.env_1 = args[0];
	    next.closure = c;
	    next.args = new Object[] { new Long(counter - 1) };
	    return;
	}
    }
}

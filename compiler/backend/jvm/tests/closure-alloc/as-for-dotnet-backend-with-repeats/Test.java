import java.util.Date;

public class Test {
    public static void main(String[] args) {
	Long counter = new Long(args[0]);
	Trampoline t = new Trampoline();
	long startTime = new Date().getTime();

	Object result;
	for (int i = 0; i < 1000; i++) {
	    result = t.call(new C1(), new Object[] { counter })[0];
	}

	long stopTime = new Date().getTime();
	System.out.println((stopTime - startTime) / (double) 1000);
	System.out.println(counter.longValue() / ((stopTime - startTime) / (double) 1000));
    }
}
import java.util.Date;

public class Test2 {
    public static final int stackDepth = 10;

    public static Long nextArg;

    public static void countDown(int stackLeft, Long arg) {
	if (stackLeft == 0) {
	    nextArg = arg;
	    return;
	}

	long counter = ((Long)arg).longValue();
	if (counter == 0) {
	    nextArg = null;
	    return;
	} else {
	    countDown(stackLeft - 1, new Long(counter - 1));
	    return;
	}
    }

    public static Long driver(Long arg) {
	nextArg = arg;
	while (nextArg != null) {
	    countDown(stackDepth, nextArg);
	}
	return null;
    }

    public static void main(String[] args) {
	Long counter = new Long(args[0]);
	long startTime = new Date().getTime();

	Object result = driver(counter);

	long stopTime = new Date().getTime();
	System.out.println((stopTime - startTime) / (double) 1000);
	System.out.println(counter.longValue() / ((stopTime - startTime) / (double) 1000));
    }
}
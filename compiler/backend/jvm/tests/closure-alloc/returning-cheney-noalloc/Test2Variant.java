import java.util.Date;

public class Test2Variant {
    public static final int stackDepth = 100;

    public static Long nextArg;

    public static void countDown_internal(int stackLeft, long arg) {
	if (stackLeft == 0) {
	    nextArg = new Long(arg);
	    return;
	}

	if (arg == 0) {
	    nextArg = null;
	    return;
	} else {
	    countDown_internal(stackLeft - 1, arg - 1);
	    return;
	}
    }

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
	    countDown_internal(stackLeft - 1, counter - 1);
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
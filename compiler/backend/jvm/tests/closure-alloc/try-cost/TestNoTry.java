import java.util.Date;

public class TestNoTry {
    public static Long driver(Long arg) {
	long counter = ((Long)arg).longValue();
	if (counter == 0)
	    return arg;
	return driver(new Long(counter - 1));
    }

    public static void main(String[] args) {
	Long counter = new Long(args[0]);
	long startTime = new Date().getTime();

	Object result;
	for (int i = 0; i < 1000; i++) {
	    result = driver(counter);
	}

	long stopTime = new Date().getTime();
	System.out.println((stopTime - startTime) / (double) 1000);
	System.out.println(counter.longValue() / ((stopTime - startTime) / (double) 1000));
    }
}
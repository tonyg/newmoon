import java.util.Date;

public class Test3 {
    public static long countDown(long counter) {
	if (counter == 0) {
	    return 0;
	} else {
	    return counter - 1;
	}
    }

    public static void driver(long arg) {
	while (arg != 0) {
	    arg = countDown(arg);
	}
    }

    public static void main(String[] args) {
	long counter = new Long(args[0]).longValue();
	Trampoline t = new Trampoline();
	long startTime = new Date().getTime();

	for (int i = 0; i < 1000; i++) {
	    driver(counter);
	}

	long stopTime = new Date().getTime();
	System.out.println((stopTime - startTime) / (double) 1000);
	System.out.println(counter / ((stopTime - startTime) / (double) 1000));
    }
}
import java.util.Date;

public class Test4 {
    public static void driver(long arg) {
	while (arg != 0) {
	    arg = arg - 1;
	}
    }

    public static void main(String[] args) {
	long counter = new Long(args[0]).longValue();
	Trampoline t = new Trampoline();
	long startTime = new Date().getTime();

	driver(counter);

	long stopTime = new Date().getTime();
	System.out.println((stopTime - startTime) / (double) 1000);
	System.out.println(counter / ((stopTime - startTime) / (double) 1000));
    }
}
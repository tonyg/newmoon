public class T {
    public int myField;

    public void toggle() {
	myField = -myField;
    }

    public static void doMap(Foo.Bar b, int n) {
	int m = b.mapper(n);
	System.out.println("Mapping " + n + " --> " + m);
    }

    public static void main(String [] args) {
	System.out.print("xyzzy: ");
	System.out.println(Foo.Bar.xyzzy);
	Foo.Bar b = new Foo.Bar();
	System.out.println("Bar prints as: " + b.toString());
	doMap(b, 1);
	doMap(b, 2);
	doMap(b, 3);
	doMap(b, 4);
	doMap(b, 5);
    }
}
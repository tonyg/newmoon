public class A {
    int x;

    public A(int x) {
	this.x = x;
    }

    public virtual int getX() {
	return this.x;
    }

    public int getX2() {
        return this.x;
    }

    public static int fiddle(int b) {
        return b * 3;
    }
}

public class A {
    int x;
    public static int yy;
    public static object bval;

    public A(int x) {
	this.x = x;
    }

    public virtual int getX() {
	return ((bool) bval) ? this.x : 999;
    }

    public int getX2() {
        return this.x;
    }

    public static int fiddle(int b) {
	object x = true;
	string y = x.ToString();
	if (yy == 34) {
	    return b * 3;
	} else {
	    return b * 600;
	}
    }
}

public class TInner {
    public class Inner1 {
	public int x;
    }
    public class Inner2 {
	public int y;
    }

    public Object make1() {
	Inner1 i = new Inner1();
	i.x = 123;
	return i;
    }
}
namespace MyNamespace {
public class Zot
{
    public static void RecCS(int n) {
	if (n == 0) {
	    Die();
	} else {
	    RecCS(n - 1);
	}
    }

    public static void Die()
    {
	throw new System.ApplicationException("Argh");
    }
}
}
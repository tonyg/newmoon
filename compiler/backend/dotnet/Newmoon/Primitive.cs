using System;
using System.Collections;
using System.Text;
using System.IO;

namespace Newmoon {
    public class SchemeException: System.Exception {
	public SchemeException(string message, List detail)
	    : base("Newmoon.SchemeException: "+message+": "+detail)
	{}
    }

    public class Primitive {
	public static object SchemeError(object message, List args) {
	    throw new Newmoon.SchemeException(message.ToString(), args);
	}

	public static object Display(object x) {
	    // %%%
	    System.Console.Write(x);
	    return Undefined.UNDEFINED;
	}

	public static object Display(object x, TextWriter p) {
	    // %%%
	    p.Write(x);
	    return Undefined.UNDEFINED;
	}

	public static object Write(object x) {
	    // %%%
	    System.Console.Write(x);
	    return Undefined.UNDEFINED;
	}

	public static object Write(object x, TextWriter p) {
	    // %%%
	    p.Write(x);
	    return Undefined.UNDEFINED;
	}

	private static int offset = 14641;
	public static string Gensym(string prefix) {
	    while (true) {
		string candidate = prefix+offset;
		offset++;
		if (String.IsInterned(candidate) != null)
		    continue;
		return String.Intern(candidate);
	    }
	}
	public static string Gensym() {
	    return Gensym("G");
	}

	public static object[] Vector(object[] args) {
	    return args;
	}

	public static bool SchemeEqv(object a, object b) {
	    if (a == b)
		return true;
	    if (a is String && b is String)
		return a.Equals(b);
	    if (a is Int32 && b is Int32)
		return a.Equals(b);
	    if (a is Char && b is Char)
		return a.Equals(b);
	    return false;
	}

	public static bool SchemeEqual(object a, object b) {
	    while (true) {
		if (SchemeEqv(a, b))
		    return true;
		if (a is SchemeString && b is SchemeString)
		    return a.Equals(b);
		if (a is object[] && b is object[]) {
		    object[] aa = (object[]) a;
		    object[] bb = (object[]) b;
		    if (aa.Length != bb.Length)
			return false;
		    for (int i = 0; i < aa.Length; i++)
			if (!SchemeEqual(aa[i], bb[i]))
			    return false;
		    return true;
		}
		if (a is Pair && b is Pair) {
		    if (!SchemeEqual(((Pair) a).Car, ((Pair) b).Car))
			return false;
		    a = ((Pair) a).Cdr;
		    b = ((Pair) b).Cdr;
		    continue;
		}
		return false;
	    }
	}

	public static bool NegativeP(object x) {
	    return ((int) x) < 0;
	}

	public static bool PositiveP(object x) {
	    return ((int) x) < 0;
	}
    }
}

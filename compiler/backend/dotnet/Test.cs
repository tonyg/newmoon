using System;
using System.Reflection;
using Newmoon;

public class A {
    public int a;
};

public class B: A {
    public int b;
};

public class Test {
    public int GetVal(A a, A a2) {
	return a.a;
    }

    public int GetVal(B a, A a2) {
	return a.b;
    }

    public void NeedInt(int i) {
	System.Console.WriteLine(i+1);
    }

    public static int GetVal(B a, B a2) {
	return a.b;
    }

    public static void TestVA(string x, params object[] more) {
	System.Console.WriteLine(x);
    }

    public static void TestVA2(string x, object[] more) {
	System.Console.WriteLine(x);
    }

    /*
    public int GetVal(B b) {
	return b.b;
    }
    */

    private static void PlayWith(string methodName) {
	MethodInfo mi = typeof(Test).GetMethod(methodName,
					       BindingFlags.Public | BindingFlags.Static);
	System.Console.WriteLine(mi);
	foreach (ParameterInfo pi in mi.GetParameters()) {
	    System.Console.WriteLine("-  " + pi.Name + " (" + pi.Position + ")");
	    foreach (ParamArrayAttribute pa in pi.GetCustomAttributes(typeof(ParamArrayAttribute), true)) {
		System.Console.WriteLine("  -  params");
	    }
	}
    }

    public static void tryTF(object x) {
	if (x is Boolean) {
	    if ((bool) x == false) {
		System.Console.WriteLine("It's false");
	    } else {
		System.Console.WriteLine("It's true");
	    }
	} else {
	    System.Console.WriteLine("It's nonboolean");
	}
    }

    public static void Main(String[] args) {
	Type testTypeAccessCode = typeof(System.String[]);

	Pair p;

	p = new Pair(1, new Pair(2, new Pair(3, Null.NULL)));
	foreach (Object o in p) {
	    System.Console.WriteLine(o);
	}

	try {
	    p = new Pair(321, 2);
	    System.Console.WriteLine(p.Car);
	    p.Car = 123;
	    System.Console.WriteLine(p.Car);
	} catch (InvalidOperationException ioe) {
	    System.Console.WriteLine(ioe);
	}

	p = new MutablePair(321, 2);
	System.Console.WriteLine(p.Car);
	p.Car = 123;
	System.Console.WriteLine(p.Car);

	Test t = new Test();
	MethodInfo mi;

	mi = t.GetType().GetMethod("GetVal",
				   BindingFlags.Public | BindingFlags.Instance,
				   null,
				   CallingConventions.Any,
				   new Type[] { typeof(B), typeof(B) },
				   null);
	System.Console.WriteLine(mi);
	System.Console.WriteLine(mi.IsStatic);

	mi = t.GetType().GetMethod("GetVal",
				   BindingFlags.Public | BindingFlags.Static,
				   null,
				   CallingConventions.Any,
				   new Type[] { typeof(B), typeof(B) },
				   null);
	System.Console.WriteLine(mi);
	System.Console.WriteLine(mi.IsStatic);

	mi = t.GetType().GetMethod("NeedInt",
				   BindingFlags.Public | BindingFlags.Instance,
				   null,
				   CallingConventions.Any,
				   new Type[] { typeof(int) },
				   null);
	System.Console.WriteLine(mi);

	mi = t.GetType().GetMethod("NeedInt",
				   BindingFlags.Public | BindingFlags.Instance,
				   null,
				   CallingConventions.Any,
				   new Type[] { typeof(Int32) },
				   null);
	System.Console.WriteLine(mi);

	System.Console.WriteLine(Type.GetType("System.Object"));
	System.Console.WriteLine(Type.GetType("System.Object[]"));
	System.Console.WriteLine(Type.GetType("System.Void"));

	TestVA("hi");
	TestVA("hi", "there");
	TestVA("hi", "there", 1, 2, 3, new object[] { "a", "b" });

	PlayWith("TestVA");
	PlayWith("TestVA2");

	
    }
}

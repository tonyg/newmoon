using System;
using System.Collections;

namespace Newmoon {
    public class Number {
	private Number() {}

	public static object Add(object[] args) {
	    switch (args.Length) {
	      case 0: return 0;
	      case 1: return (int) args[0];
	      default: {
		  int acc = (int) args[0] + (int) args[1];
		  for (int i = 2; i < args.Length; i++) {
		      acc = acc + (int) args[i];
		  }
		  return acc;
	      }
	    }
	}

	public static object Mul(object[] args) {
	    switch (args.Length) {
	      case 0: return 1;
	      case 1: return (int) args[0];
	      default: {
		  int acc = (int) args[0] * (int) args[1];
		  for (int i = 2; i < args.Length; i++) {
		      acc = acc * (int) args[i];
		  }
		  return acc;
	      }
	    }
	}

	public static object Sub(object[] args) {
	    switch (args.Length) {
	      case 0: throw new WrongArgCount(1, 0, true);
	      case 1: return - (int) args[0];
	      default: {
		  int acc = (int) args[0] - (int) args[1];
		  for (int i = 2; i < args.Length; i++) {
		      acc = acc - (int) args[i];
		  }
		  return acc;
	      }
	    }
	}

	public static object Div(object[] args) {
	    switch (args.Length) {
	      case 0: throw new WrongArgCount(1, 0, true);
	      case 1: return 1 / (int) args[0];
	      default: {
		  int acc = (int) args[0] / (int) args[1];
		  for (int i = 2; i < args.Length; i++) {
		      acc = acc / (int) args[i];
		  }
		  return acc;
	      }
	    }
	}

	public static object IsZero(object x) {
	    return (x is int) && ((int) x == 0);
	}

	public static int And(int a, int b) {
	    return a & b;
	}
    }
}

using System;
using System.Collections;

namespace Newmoon {
    public abstract class Closure {
	public readonly Module module;

	public Module Module {
	    get { return module; }
	}

	protected Closure(Module m) {
	    this.module = m;
	}

	public abstract Closure Apply(object[] args, out object[] nextParameters);

	protected static void CheckArgc(object[] args, int expected) {
	    if (args.Length != expected) {
		// Subtract one for the continuation argument.
		// Then subtract another for the empty varargs slot.
		throw new WrongArgCount(expected - 2, args.Length - 2, false);
	    }
	}

	protected static void CheckVarArgc(object[] args, int minExpected) {
	    if (args.Length < minExpected) {
		// Subtract one for the continuation argument.
		// Then subtract another for the empty varargs slot.
		throw new WrongArgCount(minExpected - 2, args.Length - 2, true);
	    }
	}
    }
}

using System;
using System.Collections;

// These are obsolescent; a better approach is to code them directly
// in cps-value language, and have the backend emit them as real
// lambdas.
namespace Newmoon {
    public class Continuation: Closure {
	private Closure k;

	public Continuation(Module m, Closure k)
	    : base(m)
	{
	    this.k = k;
	}

	public override Closure Apply(object[] args, out object[] nextParameters) {
	    object[] np = new object[args.Length - 1];
	    for (int i = 1; i < args.Length; i++)
		np[i - 1] = args[i];

	    nextParameters = np;
	    return k;
	}
    }

    public class CallWithCurrentContinuation: Closure {
	public CallWithCurrentContinuation(Module m)
	    : base(m)
	{}

	public override Closure Apply(object[] args, out object[] nextParameters) {
	    if (args.Length != 3)
		throw new WrongArgCount(1, args.Length - 2, false);

	    Closure k1 = (Closure) args[0];
	    Closure receiver = (Closure) args[1];

	    object[] np = new object[3];
	    np[0] = k1;
	    np[1] = new Continuation(this.module, k1);

	    nextParameters = np;
	    return receiver;
	}
    }

    public class Values: Closure {
	public Values(Module m)
	    : base(m)
	{}

	public override Closure Apply(object[] args, out object[] nextParameters) {
	    object[] np = new object[args.Length - 1];
	    for (int i = 1; i < args.Length; i++)
		np[i - 1] = args[i];

	    nextParameters = np;
	    return (Closure) args[0];
	}
    }

    public class MVContinuation: Closure {
	Closure receiver;
	Closure k;

	public MVContinuation(Module m, Closure receiver, Closure k)
	    : base(m)
	{
	    this.receiver = receiver;
	    this.k = k;
	}

	public override Closure Apply(object[] args, out object[] nextParameters) {
	    object[] np = new object[args.Length + 1];
	    np[0] = k;
	    for (int i = 1; i < np.Length; i++)
		np[i] = args[i - 1];

	    nextParameters = np;
	    return receiver;
	}
    }

    public class CallWithValues: Closure {
	public CallWithValues(Module m)
	    : base(m)
	{}

	public override Closure Apply(object[] args, out object[] nextParameters) {
	    if (args.Length != 4)
		throw new WrongArgCount(2, args.Length - 2, false);

	    Closure k1 = (Closure) args[0];
	    Closure generator = (Closure) args[1];
	    Closure receiver = (Closure) args[2];

	    object[] np = new object[2];
	    np[0] = new MVContinuation(this.module, receiver, k1);

	    nextParameters = np;
	    return generator;
	}
    }
}

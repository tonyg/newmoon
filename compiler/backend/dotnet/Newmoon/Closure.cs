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

	public abstract int Arity {
	    get;
	}

	public abstract bool IsRest {
	    get;
	}

	public virtual object Apply(Closure k)
	{ throw new WrongArgCount(Arity - 1, 0, IsRest); }

	public virtual object Apply(Closure k, object a)
	{ throw new WrongArgCount(Arity - 1, 1, IsRest); }

	public virtual object Apply(Closure k, object a, object b)
	{ throw new WrongArgCount(Arity - 1, 2, IsRest); }

	public virtual object Apply(Closure k, object a, object b, object c)
	{ throw new WrongArgCount(Arity - 1, 3, IsRest); }

	public virtual object Apply(Closure k, object a, object b, object c, object d)
	{ throw new WrongArgCount(Arity - 1, 4, IsRest); }

	public virtual object ApplyVarargs(Closure k, object[] args)
	{ throw new WrongArgCount(Arity - 1, args.Length, IsRest); }
    }

    public abstract class Closure0 {
	public abstract override object Apply(Closure k);
	public abstract override object ApplyVarargs(Closure k, object[] args);
    }

    public abstract class Closure1 {
	public abstract override object Apply(Closure k, object a);
	public abstract override object ApplyVarargs(Closure k, object[] args);
    }

    public abstract class Closure2 {
	public abstract override object Apply(Closure k, object a, object b);
	public abstract override object ApplyVarargs(Closure k, object[] args);
    }

    public abstract class Closure3 {
	public abstract override object Apply(Closure k, object a, object b, object c);
	public abstract override object ApplyVarargs(Closure k, object[] args);
    }

    public abstract class Closure4 {
	public abstract override object Apply(Closure k, object a, object b, object c, object d);
	public abstract override object ApplyVarargs(Closure k, object[] args);
    }
}

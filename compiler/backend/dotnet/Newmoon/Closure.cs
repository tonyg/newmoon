using System;

namespace Newmoon {
    public abstract class Closure {
	public Module module;

	public Closure(Module module) {
	    this.module = module;
	}

	public abstract object Apply(Continuation k);
	public abstract object Apply(Continuation k, object a);
	public abstract object Apply(Continuation k, object a, object b);
	public abstract object Apply(Continuation k, object a, object b, object c);
	public abstract object Apply(Continuation k, object a, object b, object c, object d);
	public abstract object ApplyVarargs(Continuation k, object[] args);
    }

    public abstract class Continuation {
	public Module module;

	public Continuation(Module module) {
	    this.module = module;
	}

	public abstract object Reply();
	public abstract object Reply(object a);
	public abstract object Reply(object a, object b);
	public abstract object Reply(object a, object b, object c);
	public abstract object Reply(object a, object b, object c, object d);
	public abstract object ReplyVarargs(object[] args);
    }

    public abstract class TailClosure: Closure {
	public TailClosure(Module module): base(module) {}

	public override object Apply(Continuation k) {
	    return ApplyVarargs(k, new object[] {});
	}
	public override object Apply(Continuation k, object a) {
	    return ApplyVarargs(k, new object[] {a});
	}
	public override object Apply(Continuation k, object a, object b) {
	    return ApplyVarargs(k, new object[] {a, b});
	}
	public override object Apply(Continuation k, object a, object b, object c) {
	    return ApplyVarargs(k, new object[] {a, b, c});
	}
	public override object Apply(Continuation k, object a, object b, object c, object d) {
	    return ApplyVarargs(k, new object[] {a, b, c, d});
	}
    }

    public abstract class TailContinuation: Continuation {
	public TailContinuation(Module module): base(module) {}

	public override object Reply() {
	    return ReplyVarargs(new object[] {});
	}
	public override object Reply(object a) {
	    return ReplyVarargs(new object[] {a});
	}
	public override object Reply(object a, object b) {
	    return ReplyVarargs(new object[] {a, b});
	}
	public override object Reply(object a, object b, object c) {
	    return ReplyVarargs(new object[] {a, b, c});
	}
	public override object Reply(object a, object b, object c, object d) {
	    return ReplyVarargs(new object[] {a, b, c, d});
	}
    }

    public class ToplevelContinuation: Continuation {
	public ToplevelContinuation(Module module): base(module) {}

	public override object Reply() {
	    return ReplyVarargs(new object[] {});
	}
	public override object Reply(object a) {
	    return a;
	}
	public override object Reply(object a, object b) {
	    return ReplyVarargs(new object[] {a, b});
	}
	public override object Reply(object a, object b, object c) {
	    return ReplyVarargs(new object[] {a, b, c});
	}
	public override object Reply(object a, object b, object c, object d) {
	    return ReplyVarargs(new object[] {a, b, c, d});
	}
	public override object ReplyVarargs(object[] args) {
	    if (args.Length == 1) {
		return args[0];
	    } else {
		throw new WrongArgCount(1, args.Length, false, true);
	    }
	}
    }
}

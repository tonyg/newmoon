using System;
using System.IO;
using System.Reflection;
using System.Resources;

namespace Newmoon {
    public abstract class Module {
	private string name;
	internal Environment env;

	protected Module(string name, Environment env) {
	    this.name = name;
	    this.env = env;
	    env.RegisterModule(name, this);
	}

	public Binding ResolveBindingCell(string name, string kind) {
	    // Until we get proper modules with require and stuff.
	    return env.ResolveBindingCell(name, kind);
	}

	public string Name {
	    get { return name; }
	}

	public Environment Env {
	    get { return env; }
	}

	public abstract Closure GetEntryPoint();
    }
}

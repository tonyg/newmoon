using System;
using System.IO;
using System.Reflection;
using System.Collections;

namespace Newmoon {
    public class Driver {
	public int Verbosity = 0;
	public bool TraceCalls = false;
	public bool LoadBasicLibrary = true;
	private bool BasicLibraryLoaded = false;

	internal struct Trace {
	    public Closure c;
	    public object[] parameters;
	};

	private Trace[] traces = new Trace[32];
	private int tracePos = 0;

	public Driver() {
	    for (int i = 0; i < traces.Length; i++)
		traces[i] = new Trace();
	}

	private void TraceCall(Closure c, object[] parameters) {
	    traces[tracePos].c = c;
	    traces[tracePos].parameters = parameters;

	    if (Verbosity > 1) {
		DumpTrace(traces[tracePos]);
	    }

	    tracePos = (tracePos + 1) % traces.Length;
	}

	private void DumpTrace(Trace t) {
	    System.Console.Error.WriteLine("Closure: "+t.c);
	    //System.Console.Error.WriteLine("Argc: "+t.parameters.Length+" [[");
	    for (int i = 0; i < t.parameters.Length; i++) {
		object o = t.parameters[i];
		if (o == null) {
		    System.Console.Error.WriteLine(" - #!void");
		} else {
		    //System.Console.Error.WriteLine(" - ("+o.GetType()+") "+o);
		    System.Console.Error.WriteLine(" - "+o);
		}
	    }
	    //System.Console.Error.WriteLine("]]");
	}

	private void DumpTrace() {
	    bool printedHeader = false;

	    for (int count = 0; count < traces.Length; count++) {
		int i = (tracePos + count) % traces.Length;

		if (traces[i].c == null)
		    continue;

		if (!printedHeader) {
		    System.Console.Error.WriteLine(
			"-*-*- PREVIOUS "+(traces.Length - count)+" CALLS -*-*-");
		    printedHeader = true;
		}

		System.Console.Error.WriteLine("Offset "+(count - (traces.Length - 1)));
		DumpTrace(traces[i]);
	    }
	}

	public object vCallScheme(Closure c, object[] args) {
	    object[] parameters = new object[args.Length + 2];
	    parameters[0] = new ToplevelContinuation();
	    System.Array.Copy(args, 0, parameters, 1, args.Length);

	    try {
		if (TraceCalls) {
		    while (c != null) {
			TraceCall(c, parameters);
			c = c.Apply(parameters, out parameters);
		    }
		} else {
		    while (c != null) {
			c = c.Apply(parameters, out parameters);
		    }
		}
	    } catch (Exception e) {
		if (TraceCalls) {
		    System.Console.Error.WriteLine(e.ToString());
		    DumpTrace();
		}
		throw e;
	    }

	    return parameters[0];
	}

	public object CallScheme(Closure c, params object[] args) {
	    return vCallScheme(c, args);
	}

	private static void usage(int exitCode) {
	    System.Console.Write(
"Usage: Set NEWMOON_RUNTIME to be a string of command-line-equivalent flags\n"+
"       from the following:\n"+
"\n"+
"  -t   trace calls\n"+
"  -T   do not trace calls\n"+
"  -v   verbosity: increase level by one\n"+
"  -B   do not load basic-library.dll\n"+
"  -h   this message\n"+
"  -?   this message\n"+
"\n"+
"or the name of an assembly of compiled library scheme to load before\n"+
"the main program.\n"
);
	    System.Environment.Exit(exitCode);
	}

	private void EnsureLibraryLoaded(Environment e) {
	    if (!BasicLibraryLoaded) {
		if (LoadBasicLibrary) {
		    String newmoonLibpath = System.Environment.GetEnvironmentVariable("NEWMOON_LIBPATH");
		    if (newmoonLibpath == null)
			newmoonLibpath = "/Users/tonyg/src/newmoon/lib";
		    CallScheme(e.InvokeModule(newmoonLibpath + "/newmoon-lib/basic-library.scm")); // %%%
		}
		BasicLibraryLoaded = true;
	    }
	}

	public static void ProgramEntryPoint(String[] argv, System.Type moduleType) {
	    Newmoon.Environment e = new Newmoon.Environment(moduleType.FullName);
	    Driver d = new Driver();

	    String configStr = System.Environment.GetEnvironmentVariable("NEWMOON_RUNTIME");
	    String[] config = (configStr == null ? new String[0] {} : configStr.Split(' '));

	    foreach (String arg in config) {
		if (arg[0] == '-') {
		    switch (arg[1]) {
		      case 't':
			  d.TraceCalls = true;
			  break;

		      case 'T':
			  d.TraceCalls = false;
			  break;

		      case 'v':
			  d.Verbosity++;
			  break;

		      case 'B':
			  d.LoadBasicLibrary = false;
			  break;

		      case 'h':
		      case '?':
			  usage(0);
			  break;

		      default:
			  usage(1);
			  break;
		    }
		    continue;
		}

		string modpath = (Path.HasExtension(arg)
				  ? arg
				  : Path.ChangeExtension(arg, "dll"));
		string modname = Path.GetFileNameWithoutExtension(arg);

		if (!e.ModuleInvoked(modname)) {
		    d.EnsureLibraryLoaded(e);
		    d.CallScheme(e.InvokeModule(modname, modpath));
		}
	    }

	    d.EnsureLibraryLoaded(e);

	    Closure entryPoint = e.InvokeModule(moduleType);

	    SchemeString[] realArgv = new SchemeString[argv.Length];
	    for (int i = 0; i < realArgv.Length; i++)
		realArgv[i] = new SchemeString(argv[i]);
	    Environment.InstallBinding(entryPoint.Module, "argv", "global", realArgv);

	    d.CallScheme(entryPoint);
	}
    }

    internal class ToplevelContinuation: Closure {
	public ToplevelContinuation()
	    : base(null)
	{}

	public override Closure Apply(object[] args, out object[] nextParameters) {
	    if (args.Length != 2)
		throw new WrongArgCount(1, args.Length - 1, false);

	    nextParameters = new object[1] { args[0] };
	    return null;
	}
    }
}

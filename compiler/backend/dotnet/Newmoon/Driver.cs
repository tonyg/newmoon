using System;
using System.IO;
using System.Reflection;
using System.Collections;

namespace Newmoon {
    public class Driver {
	public object vCallScheme(Closure c, object[] args) {
	    return c.ApplyVarargs(new ToplevelContinuation(), args);
	}

	public object CallScheme(Closure c, params object[] args) {
	    return vCallScheme(c, args);
	}

	private static void usage(int exitCode) {
	    System.Console.Write(
"Usage: Set NEWMOON_RUNTIME to be a string of command-line-equivalent flags\n"+
"       from the following:\n"+
"\n"+
"  -h   this message\n"+
"  -?   this message\n"+
"\n"+
"or the name of an assembly of compiled library scheme to load before\n"+
"the main program.\n"
);
	    System.Environment.Exit(exitCode);
	}

	public static void ProgramEntryPoint(String[] argv, System.Type moduleType) {
	    Newmoon.Environment e = new Newmoon.Environment(moduleType.FullName);
	    Driver d = new Driver();

	    String configStr = System.Environment.GetEnvironmentVariable("NEWMOON_RUNTIME");
	    String[] config = (configStr == null ? new String[0] {} : configStr.Split(' '));

	    foreach (String arg in config) {
		if (arg[0] == '-') {
		    switch (arg[1]) {
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
		    d.CallScheme(e.InvokeModule(modname, modpath));
		}
	    }

	    Closure entryPoint = e.InvokeModule(moduleType);

	    SchemeString[] realArgv = new SchemeString[argv.Length];
	    for (int i = 0; i < realArgv.Length; i++)
		realArgv[i] = new SchemeString(argv[i]);
	    Environment.InstallBinding(null, "argv", "global", realArgv);

	    d.CallScheme(entryPoint);
	}
    }
}

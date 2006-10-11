using System;
using System.Text;
using System.IO;
using System.Collections;
using System.Reflection;
using System.Reflection.Emit;

namespace Newmoon {
    public class Environment {
	IDictionary assemblies;
	ArrayList assemblyList;
	IDictionary modules;
	SchemeString description;
	Hashtable bindings;

	public Environment() : this("Anonymous") {}

	public Environment(string desc) : this(new SchemeString(desc)) {}

	public Environment(SchemeString desc) {
	    assemblies = new Hashtable();
	    assemblyList = new ArrayList();
	    modules = new Hashtable();
	    bindings = new Hashtable();
	    description = new SchemeString("Newmoon.Environment("+desc+")");
	}

	public Binding ResolveBindingCell(string name, string kind, bool complainIfMissing) {
	    Binding result = null;
	    if (bindings.Contains(name)) {
		result = (Binding) bindings[name];
		if (result.kind != kind) {
		    throw new Exception("Expected binding kind "+kind+" for "+name+
					"; got "+result.kind);
		}
	    } else {
                if (complainIfMissing) {
                    System.Console.Error.WriteLine(";; Warning: Uninitialized \"" +
                                                   kind + "\" binding: " + name);
                }
		Binding b = new Binding(name, kind);
		bindings[name] = b;
		result = b;
	    }
	    return result;
	}

	public static void InstallBinding(Newmoon.Module mod,
					  object name,
					  string kind,
					  object val)
	{
            System.Console.WriteLine(";; Installing \"" + kind + "\" binding: "+name);
	    switch (kind) {
	      case "global": {
		  Binding b = mod.ResolveBindingCell((string) name, kind, false);
		  b.value = val;
		  break;
	      }

	      case "primitive": {
		  //System.Console.WriteLine(";; Installing primitive: "+val); %%%
		  break;
	      }

              case "macro": {
                  break;
              }

	      default:
		  throw new Exception("Unsupported binding-kind "+kind+" for "+name);
	    }
	}

	private Assembly InstallAssembly(string assemblyPath) {
	    if (!assemblies.Contains(assemblyPath)) {
		Assembly a = Assembly.LoadFrom(assemblyPath);
		if (a != null) {
		    assemblies.Add(assemblyPath, a);
		    assemblyList.Add(a);
		}
		return a;
	    } else {
		return (Assembly) assemblies[assemblyPath];
	    }
	}

	public Module InvokeModule(string arg) {
	    string modpath = Path.ChangeExtension(arg, "dll");
	    string modname = Path.GetFileNameWithoutExtension(arg);
	    return InvokeModule(modname, modpath);
	}

	public Module InvokeModule(string modname, string modpath) {
	    System.Console.WriteLine("Invoking "+modname+" from "+modpath+"...");

	    if (modules.Contains(modname)) {
		throw new Exception("Cannot reinvoke module within the same Environment");
	    }

	    Assembly a = InstallAssembly(modpath);
	    if (a == null) {
		throw new Exception("Could not load assembly "+modpath);
	    }

	    Type t = a.GetType("Newmoon.CompiledModules."+modname+"._"+modname);
	    if (t == null) {
		throw new Exception("Couldn't resolve Scheme entrypoint in "+modpath);
	    }

	    return InvokeModule(t);
	}

	public Module InvokeModule(Type t) {
	    Newmoon.Environment[] cArgs = new Newmoon.Environment[1] { this };
	    Type[] cTypes = Type.GetTypeArray(cArgs);

	    Newmoon.Module m = (Newmoon.Module) t.GetConstructor(cTypes).Invoke(cArgs);
            return m;
	}

	public bool ModuleInvoked(string modname) {
	    return modules.Contains(modname);
	}

	public Module LookupModule(string name) {
	    return (Module) modules[name];
	}

	public void RegisterModule(string name, Module mod) {
	    modules[name] = mod;
	}

	public Type GetType(string typeName) {
	    Type t = Type.GetType(typeName);
	    if (t != null)
		return t;
	    foreach (Assembly a in assemblyList) {
		t = a.GetType(typeName);
		if (t != null)
		    return t;
	    }
	    throw new Exception("No type found in environment "+this+" for "+typeName);
	}
    }
}

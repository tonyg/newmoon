using System;
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

	public Binding ResolveBindingCell(string name, string kind) {
	    Binding result = null;
	    if (bindings.Contains(name)) {
		result = (Binding) bindings[name];
		if (result.kind != kind) {
		    throw new Exception("Expected binding kind "+kind+" for "+name+
					"; got "+result.kind);
		}
	    } else {
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
	    switch (kind) {
	      case "global": {
		  Binding b = mod.ResolveBindingCell((string) name, kind);
		  b.value = val;
		  break;
	      }

	      case "primitive": {
		  //System.Console.WriteLine(";; Installing primitive: "+val); %%%
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

	public Closure InvokeModule(string arg) {
	    string modpath = Path.ChangeExtension(arg, "dll");
	    string modname = Path.GetFileNameWithoutExtension(arg);
	    return InvokeModule(modname, modpath);
	}

	public Closure InvokeModule(string modname, string modpath) {
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

	public Closure InvokeModule(Type t) {
	    Newmoon.Environment[] cArgs = new Newmoon.Environment[1] { this };
	    Type[] cTypes = Type.GetTypeArray(cArgs);

	    Newmoon.Module m = (Newmoon.Module) t.GetConstructor(cTypes).Invoke(cArgs);
	    return m.GetEntryPoint();
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

	public static object InvokeGenericStatic(Closure requestor,
						 string typeName,
						 string methodName,
						 Object[] args)
	{
	    Environment e = requestor.module.env;
	    Type t = e.GetType(typeName);

	    Type[] types = Type.GetTypeArray(args);
	    MethodInfo mi = t.GetMethod(methodName,
					BindingFlags.Public |
					BindingFlags.Static,
					null,
					types,
					null);
	    if (mi == null) {
		throw new Exception("No suitable method found for "+t+" method "+
				    methodName+"/"+args);
	    } else {
		object result = mi.Invoke(null, args);
		if (result == null)
		    return Undefined.UNDEFINED;
		else
		    return result;
	    }
	}

	public static object InvokeVarargsStatic(Closure requestor,
						 string typeName,
						 string methodName,
						 Object[] args)
	{
	    Environment e = requestor.module.env;
	    Type t = e.GetType(typeName);

	    MethodInfo mi = t.GetMethod(methodName,
					BindingFlags.Public |
					BindingFlags.Static,
					null,
					new Type[1] { typeof(object[]) },
					null);
	    if (mi == null) {
		throw new Exception("No suitable method found for "+t+" varargs method "+
				    methodName+"/"+args);
	    } else {
		object result = mi.Invoke(null, new Object[1] { args });
		if (result == null)
		    return Undefined.UNDEFINED;
		else
		    return result;
	    }
	}

	public static object InvokeGenericMethod(Closure requestor,
						 string methodName,
						 Object[] p)
	{
	    Environment e = requestor.module.env;
	    Object receiver = p[0];
	    Type t = receiver.GetType();

	    Object[] args = new Object[p.Length - 1];
	    for (int i = 0; i < args.Length; i++)
		args[i] = p[i+1];

	    Type[] types = Type.GetTypeArray(args);
	    MethodInfo mi = t.GetMethod(methodName,
					BindingFlags.Public |
					BindingFlags.Instance,
					null,
					types,
					null);

	    if (mi == null) {
		throw new Exception("No suitable method found for "+t+" method "+
				    methodName+"/"+List.FromVector(types));
	    } else {
		object result = mi.Invoke(receiver, args);
		if (result == null)
		    return Undefined.UNDEFINED;
		else
		    return result;
	    }
	}
    }

    public class InstallBindingAssembler: Newmoon.IAssembler {
	public AssemblerState Assemble(ILGenerator ilg,
				       ICodeGenerator cg,
				       Pair node,
				       List flags,
				       string name,
				       List args)
	{
	    List rands = (List) cg.NodeGet(node, "extern-apply", "rands");
	    int argc = rands.ListLength();

	    if (argc != 3) {
		throw new Exception("sys$install-binding needs exactly three arguments, not "+
				    argc);
	    }

	    object nameNode = ((Pair) rands).Car;
	    object kindNode = ((Pair) rands).D.Car;
	    object valueNode = ((Pair) rands).D.D.Car;

	    ilg.Emit(OpCodes.Ldarg_0);
	    ilg.Emit(OpCodes.Ldfld,
		     typeof(Newmoon.Closure).GetField("module",
							  BindingFlags.Public |
							  BindingFlags.Instance));

	    if (cg.GenNode(ilg, nameNode) != AssemblerState.VALUE)
		throw new Exception("Name subexpression must return a value");

	    if (cg.GenNode(ilg, kindNode) != AssemblerState.VALUE)
		throw new Exception("Kind subexpression must return a value");

	    cg.PushDotNetArg(ilg, valueNode, null, 1);
	    ilg.Emit(OpCodes.Call,
		     typeof(Newmoon.Environment).GetMethod("InstallBinding",
							       BindingFlags.Public |
							       BindingFlags.Static,
							       null,
							       new Type[4] {
								   typeof(Newmoon.Module),
								   typeof(object),
								   typeof(string),
								   typeof(object)
							       },
							       null));
	    return AssemblerState.NOVALUE;
	}
    }
}

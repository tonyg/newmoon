using System;
using System.IO;
using System.Collections;
using System.Resources;
using System.Reflection;
using System.Reflection.Emit;
using Newmoon;

public class Sil: ICodeGenerator {
    public static void Main(String[] args) {
	Sil s = new Sil(args);
	s.Run();
    }

    private string inputFilename = null;
    private string assemblyName = null;
    private ArrayList assemblies = new ArrayList();
    private bool instructionTraces = false;
    private bool pauseTracing = false;
    private bool traceValues = false;
    private bool ctTrace = true;
    private bool makeExe = false;
    public static bool suppress1 = false;

    private void usage(int exitCode) {
	System.Console.WriteLine(
		"Usage: sil -o assemblyname [-dpvxtT] [-a assembly ...] inputfile");
	System.Console.WriteLine("");
	System.Console.WriteLine("Note: assemblyname shouldn't have any suffix - "+
				 ".dll or .exe will be added.");
	System.Environment.Exit(exitCode);
    }

    private Sil(String[] args) {
	for (int i = 0; i < args.Length; i++) {
	    if (args[i][0] == '-') {
		switch (args[i][1]) {
		  case 'o':
		      assemblyName = args[++i];
		      break;

		  case 'd':
		      instructionTraces = true;
		      break;

		  case 'p':
		      instructionTraces = true;
		      pauseTracing = true;
		      break;

		  case 'v':
		      instructionTraces = true;
		      traceValues = true;
		      break;

		  case 'a': {
		      Assembly a = Assembly.LoadFrom(args[++i]);
		      assemblies.Add(a);
		      break;
		  }

		  case 'x':
		      makeExe = true;
		      break;

		  case 't':
		      ctTrace = true;
		      break;

		  case 'T':
		      ctTrace = false;
		      break;

		  case 'h':
		  case '?':
		      usage(0);
		      break;

		  default:
		      usage(1);
		      break;
		}
	    } else {
		inputFilename = args[i];
	    }
	}

	if (inputFilename == null ||
	    assemblyName == null) {
	    usage(1);
	}
    }

    private void Run() {
	TextReader input = new StreamReader(inputFilename);
	Pair sil = (Pair) Newmoon.Reader.Read(input);
	// .sil files contain: ((visit-time-expr ...) parse-tree)
	// We operate on the parse-tree and ignore the visit-time expressions.
	GenCodeForModule(sil.D.Car);
    }

    ///////////////////////////////////////////////////////////////////////////

    private string NodeKind(object node) {
	return (string) ((Pair)node).Car;
    }

    private object NodeGetOrFalse(object node, string kind, string key) {
	if (!kind.Equals(NodeKind(node)))
	    throw new Exception("Wrong node-kind: "+kind+"/"+NodeKind(node));

	foreach (Pair cell in ((Pair)node).D) {
	    if (((string)cell.Car).Equals(key))
		return cell.D.Car;
	}

	return false;
    }

    private List NodeGetOrEmpty(object node, string kind, string key) {
	object r = NodeGetOrFalse(node, kind, key);
	if (false.Equals(r))
	    return Null.NULL;
	return (List) r;
    }

    public object NodeGet(object node, string kind, string key) {
	if (!kind.Equals(NodeKind(node)))
	    throw new Exception("Wrong node-kind: "+kind+"/"+NodeKind(node));

	foreach (Pair cell in ((Pair)node).D) {
	    if (((string)cell.Car).Equals(key))
		return cell.D.Car;
	}

	throw new Exception("No key in node: "+kind+"/"+key);
    }

    ///////////////////////////////////////////////////////////////////////////

    private AssemblyBuilder assemblyBuilder;
    private ModuleBuilder moduleBuilder;
    private TypeBuilder entryPoint;
    private String fqNamespace;
    private TypeBuilder currentLambda = null;
    private FieldBuilder[] currentEnv = null;
    private Hashtable bindings = new Hashtable();
    private List vecLocals = Null.NULL;

    private static Type voidType = typeof(void);

    private static MethodInfo stringIntern = typeof(String).GetMethod("Intern",
								      BindingFlags.Public |
								      BindingFlags.Static,
								      null,
								      new Type[1] {typeof(String)},
								      null);

    private static ConstructorInfo symbolToString =
	typeof(Newmoon.SchemeString).GetConstructor(BindingFlags.Public |
							BindingFlags.Instance,
							null,
							new Type[2] {
							    typeof(string),
							    typeof(bool)
							},
							null);

    private static ConstructorInfo pairCons = typeof(Pair).GetConstructor(new Type[2] {
	typeof(object),
	typeof(object)
    });

    private static ConstructorInfo mutableCons = typeof(MutablePair).GetConstructor(new Type[2] {
	typeof(object),
	typeof(object)
    });

    private static ConstructorInfo undefinedGlobal =
	typeof(UndefinedGlobalVariable).GetConstructor(BindingFlags.Public |
						       BindingFlags.Instance,
						       null,
						       new Type[1] {
							   typeof(Binding)
						       },
						       null);

    private static ConstructorInfo cellConstructor =
	typeof(Newmoon.Cell).GetConstructor(BindingFlags.Public |
						BindingFlags.Instance,
						null,
						new Type[1] {
						    typeof(object)
						},
						null);

    private static MethodInfo closureCheckArgc =
	typeof(Newmoon.Closure).GetMethod("CheckArgc",
					      BindingFlags.NonPublic |
					      BindingFlags.Static,
					      null,
					      new Type[2] {
						  typeof(object[]),
						  typeof(int)
					      },
					      null);

    private static MethodInfo closureCheckVarArgc =
	typeof(Newmoon.Closure).GetMethod("CheckVarArgc",
					      BindingFlags.NonPublic |
					      BindingFlags.Static,
					      null,
					      new Type[2] {
						  typeof(object[]),
						  typeof(int)
					      },
					      null);

    private static MethodInfo pairXcons =
	typeof(Newmoon.Pair).GetMethod("xcons",
					   BindingFlags.Public |
					   BindingFlags.Static,
					   null,
					   new Type[2] {
					       typeof(object),
					       typeof(object)
					   },
					   null);

    private static MethodInfo environmentInvokeGenericStatic =
	typeof(Newmoon.Environment).GetMethod("InvokeGenericStatic",
						  new Type[4] {
						      typeof(Closure),
						      typeof(string),
						      typeof(string),
						      typeof(Object[])
						  });

    private static MethodInfo environmentInvokeVarargsStatic =
	typeof(Newmoon.Environment).GetMethod("InvokeVarargsStatic",
						  new Type[4] {
						      typeof(Closure),
						      typeof(string),
						      typeof(string),
						      typeof(Object[])
						  });

    private static MethodInfo environmentInvokeGenericMethod =
	typeof(Newmoon.Environment).GetMethod("InvokeGenericMethod",
						  new Type[3] {
						      typeof(Closure),
						      typeof(string),
						      typeof(Object[])
						  });

    private static FieldInfo nullNullField =
	typeof(Newmoon.Null).GetField("NULL",
					  BindingFlags.Public |
					  BindingFlags.Static);

    private static FieldInfo undefField =
	typeof(Newmoon.Undefined).GetField("UNDEFINED",
					       BindingFlags.Public |
					       BindingFlags.Static);

    private static FieldInfo variableValue =
	typeof(Newmoon.Binding).GetField("value",
					     BindingFlags.Public |
					     BindingFlags.Instance);

    private static FieldInfo cellValue =
	typeof(Newmoon.Cell).GetField("value",
					  BindingFlags.Public |
					  BindingFlags.Instance);

    private static FieldInfo closureModule =
	typeof(Newmoon.Closure).GetField("module",
					     BindingFlags.Public |
					     BindingFlags.Instance);

    private String prefixed(String s) {
	return fqNamespace + "." + s;
    }

    private void GenLdClosure(ILGenerator ilg) {
	ilg.Emit(OpCodes.Ldarg_0);
    }

    private void GenLdArgVec(ILGenerator ilg) {
	ilg.Emit(OpCodes.Ldarg_1);
    }

    private void GenLdParamAddr(ILGenerator ilg) {
	ilg.Emit(OpCodes.Ldarg_2);
    }

    private void GenCodeForModule(object node) {
	String assemblyDir = Path.GetDirectoryName(assemblyName);
	if ("".Equals(assemblyDir))
	    assemblyDir = ".";

	String suffix = makeExe ? "exe" : "dll";

	String assemblyFilename = Path.GetFileName(Path.HasExtension(assemblyName)
						   ? assemblyName
						   : Path.ChangeExtension(assemblyName, suffix));
	assemblyName = Path.GetFileNameWithoutExtension(assemblyName);

	System.Console.WriteLine(";; dotnet backend generating "+assemblyFilename+"...");

	AssemblyName aName = new AssemblyName();
	aName.Name = assemblyName;
	fqNamespace = "Newmoon.CompiledModules."+assemblyName;

	assemblyBuilder =
	    AppDomain.CurrentDomain.DefineDynamicAssembly(aName,
							  AssemblyBuilderAccess.Save,
							  assemblyDir);

	try {
	    moduleBuilder = assemblyBuilder.DefineDynamicModule(prefixed(assemblyName),
								assemblyFilename,
								false);
	} catch (System.NotSupportedException nse) {
	    // Portable.NET doesn't yet support the above variable of
	    // DefineDynamicModule. Try a different one.
	    moduleBuilder = assemblyBuilder.DefineDynamicModule(prefixed(assemblyName),
								false);
	}

	entryPoint = moduleBuilder.DefineType(prefixed("_"+assemblyName),
					      TypeAttributes.Public,
					      typeof(Newmoon.Module));

	ConstructorBuilder closureBuilder = GenClosure(node, true);

	///////////////////////////////////////////////////////////////////////////
	// Build instance constructor

	ConstructorBuilder cB =
	    entryPoint.DefineConstructor(MethodAttributes.Public |
					 MethodAttributes.HideBySig,
					 CallingConventions.Standard,
					 new Type[1] {typeof(Newmoon.Environment)});

	ILGenerator ilg = cB.GetILGenerator();
	List oldLocals = pushVecLocals();

	// Call superclass constructor
	//
	ilg.Emit(OpCodes.Ldarg_0);		// this Module
	ilg.Emit(OpCodes.Ldstr, assemblyName);	// assemblyname
	ilg.Emit(OpCodes.Ldarg_1);		// environment
	ConstructorInfo cInfo =
	    typeof(Newmoon.Module).GetConstructor(BindingFlags.NonPublic |
						      BindingFlags.Instance,
						      null,
						      new Type[2] {
							  typeof(string),
							  typeof(Newmoon.Environment)
						      },
						      null);
	ilg.Emit(OpCodes.Call, cInfo);

	// Build binding fields
	//
	foreach (string bindingName in bindings.Keys) {
	    Trace("Binding for "+bindingName+": "+bindings[bindingName]);
	    // :
	    ilg.Emit(OpCodes.Ldarg_0);	// this Module
	    ilg.Emit(OpCodes.Dup);
	    ilg.Emit(OpCodes.Ldstr, bindingName);
	    // : module module string
	    ilg.Emit(OpCodes.Call, stringIntern);
	    // : module module symbol
	    ilg.Emit(OpCodes.Ldstr, "global");
	    // : module module symbol string
	    ilg.Emit(OpCodes.Call, stringIntern);
	    // : module module symbol symbol
	    ilg.Emit(OpCodes.Callvirt,
		     typeof(Newmoon.Module).GetMethod("ResolveBindingCell",
							  new Type[2] {
							      typeof(string),
							      typeof(string)
							  }));
	    // : module binding
	    ilg.Emit(OpCodes.Stfld, (FieldBuilder) bindings[bindingName]);
	    // :
	}

	// Return from constructor
	//
	ilg.Emit(OpCodes.Ret);
	popVecLocals(oldLocals);

	///////////////////////////////////////////////////////////////////////////
	// Build library entry point method

	MethodBuilder gepB =
	    entryPoint.DefineMethod("GetEntryPoint",
				    MethodAttributes.Public |
				    MethodAttributes.Virtual |
				    MethodAttributes.HideBySig,
				    typeof(Newmoon.Closure),
				    new Type[0] {});
	ilg = gepB.GetILGenerator();
	oldLocals = pushVecLocals();

	// Build instance of module initialisation closure.
	// It will have no captures (being a root lambda).
	// It will also require us to have no arguments.
	//
	ilg.Emit(OpCodes.Ldarg_0);	// this Module
	ilg.Emit(OpCodes.Newobj, closureBuilder);
	ilg.Emit(OpCodes.Ret);
	popVecLocals(oldLocals);

	///////////////////////////////////////////////////////////////////////////
	// If makeExe, build program entry point method

	if (makeExe) {
	    MethodBuilder pepB =
		entryPoint.DefineMethod("Main",
					MethodAttributes.Public |
					MethodAttributes.Static |
					MethodAttributes.HideBySig,
					null,
					new Type[1] {typeof(System.String[])});
	    ilg = pepB.GetILGenerator();
	    oldLocals = pushVecLocals();

	    ilg.Emit(OpCodes.Ldarg_0);	// argv
	    ilg.Emit(OpCodes.Ldtoken, entryPoint);
	    ilg.Emit(OpCodes.Call,
		     typeof(System.Type).GetMethod("GetTypeFromHandle",
						   BindingFlags.Public |
						   BindingFlags.Static,
						   null,
						   new Type[1] {typeof(System.RuntimeTypeHandle)},
						   null));
	    ilg.Emit(OpCodes.Callvirt,
		     typeof(Newmoon.Driver).GetMethod("ProgramEntryPoint",
						      BindingFlags.Public |
						      BindingFlags.Static,
						      null,
						      new Type[2] {
							  typeof(System.String[]),
							  typeof(System.Type)
						      },
						      null));
	    ilg.Emit(OpCodes.Ret);
	    popVecLocals(oldLocals);

	    assemblyBuilder.SetEntryPoint(pepB);
	}

	///////////////////////////////////////////////////////////////////////////
	// Finalize types and assemblies, and output the result.

	entryPoint.CreateType();
	assemblyBuilder.Save(assemblyFilename);
	System.Console.WriteLine(";; dotnet backend finished generating "+assemblyFilename+".");
    }

    private int bindingCounter = 0;
    public FieldBuilder RecordBinding(string name) {
	if (bindings.Contains(name)) {
	    return (FieldBuilder) bindings[name];
	} else {
	    string bName = "binding_"+name+"_"+(bindingCounter++);
	    FieldBuilder bB = entryPoint.DefineField(bName, typeof(Newmoon.Binding),
						     FieldAttributes.Assembly);
	    bindings[name] = bB;
	    return bB;
	}
    }

    internal LocalBuilder getVecLocal(ILGenerator ilg) {
	if (vecLocals == Null.NULL)
	    return ilg.DeclareLocal(typeof(System.Object[]));
	else {
	    LocalBuilder lb = (LocalBuilder) ((Pair) vecLocals).Car;
	    vecLocals = (List) ((Pair) vecLocals).Cdr;
	    return lb;
	}
    }

    internal void returnVecLocal(LocalBuilder lb) {
	vecLocals = new Pair(lb, vecLocals);
    }

    internal List pushVecLocals() {
	List result = vecLocals;
	vecLocals = Null.NULL;
	return result;
    }

    internal void popVecLocals(List old) {
	vecLocals = old;
    }

    public void GenLiteralCode(ILGenerator ilg, object literal) {
	Trace("_GenLiteral for "+literal.GetType()+" ("+literal+")");
	indent += 2;

	MethodInfo mi = typeof(Sil).GetMethod("_GenLiteral",
					      BindingFlags.NonPublic |
					      BindingFlags.Instance,
					      null,
					      new Type[2] {
						  typeof(ILGenerator),
						  literal.GetType()
					      },
					      null);
	if (mi == null) {
	    throw new Exception("Unsupported literal: "+literal.GetType());
	} else {
	    mi.Invoke(this, new Object[2] { ilg, literal });
	}

	indent -= 2;
    }

    internal void _GenLiteral(ILGenerator ilg, SchemeString literal) {
	ilg.Emit(OpCodes.Ldstr, literal.ToString());
	ilg.Emit(OpCodes.Ldc_I4_0);
	ilg.Emit(OpCodes.Newobj, symbolToString);
    }

    internal void _GenLiteral(ILGenerator ilg, int literal) {
	ilg.Emit(OpCodes.Ldc_I4, (int) literal);
	ilg.Emit(OpCodes.Box, typeof(System.Int32));
    }

    internal void _GenLiteral(ILGenerator ilg, char literal) {
	ilg.Emit(OpCodes.Ldc_I4, (char) literal);
	ilg.Emit(OpCodes.Box, typeof(System.Char));
    }

    internal void _GenLiteral(ILGenerator ilg, object[] literal) {
	LocalBuilder vb = ilg.DeclareLocal(typeof(System.Object[]));
	ilg.Emit(OpCodes.Ldc_I4, literal.Length);
	ilg.Emit(OpCodes.Newarr, typeof(System.Object));
	ilg.Emit(OpCodes.Stloc, vb);
	int counter = 0;
	foreach (object e in literal) {
	    ilg.Emit(OpCodes.Ldloc, vb);
	    ilg.Emit(OpCodes.Ldc_I4, counter++);
	    GenLiteralCode(ilg, e);
	    ilg.Emit(OpCodes.Stelem_Ref);
	}
	ilg.Emit(OpCodes.Ldloc, vb);
    }

    internal void _GenLiteral(ILGenerator ilg, Pair literal) {
	GenLiteralCode(ilg, literal.Cdr);
	GenLiteralCode(ilg, literal.Car);
	ilg.Emit(OpCodes.Call, pairXcons);
    }

    internal void _GenLiteral(ILGenerator ilg, Null literal) {
	ilg.Emit(OpCodes.Ldsfld, nullNullField);
    }

    internal void _GenLiteral(ILGenerator ilg, Undefined literal) {
	ilg.Emit(OpCodes.Ldsfld, undefField);
    }

    internal void _GenLiteral(ILGenerator ilg, string literal) {
	ilg.Emit(OpCodes.Ldstr, literal);
	ilg.Emit(OpCodes.Call, stringIntern);
    }

    internal void _GenLiteral(ILGenerator ilg, bool literal) {
	if (literal) {
	    ilg.Emit(OpCodes.Ldc_I4_1);
	} else {
	    ilg.Emit(OpCodes.Ldc_I4_0);
	}
	ilg.Emit(OpCodes.Box, typeof(Boolean));
    }

    private int indent = 0;
    private void Trace(object msg) {
	if (ctTrace)
	    Trace(msg, 0);
    }
    private void Trace(object msg, int extra) {
	if (ctTrace) {
	    for (int i = 0; i < indent + extra; i++)
		System.Console.Write(' ');
	    System.Console.WriteLine(msg);
	}
    }

    public AssemblerState GenNode(ILGenerator ilg, object node) {
	AssemblerState retval;

	string kind = NodeKind(node).Replace('-', '_');
	Trace(kind);
	indent += 2;

	if (instructionTraces) {
	    TraceStrI(ilg, ">>> TRACE: before "+kind);
	    if (pauseTracing) {
		ilg.Emit(OpCodes.Call,
			 typeof(System.Console).GetMethod("ReadLine"));
		ilg.Emit(OpCodes.Pop);
	    }
	}

	MethodInfo mi = typeof(Sil).GetMethod("Gen_"+kind,
					      new Type[2] {
						  typeof(ILGenerator),
						  typeof(object)
					      });
	if (mi == null) {
	    throw new Exception("Unknown node kind in GenNode: "+kind);
	} else {
	    retval = (AssemblerState) mi.Invoke(this, new Object[2] { ilg, node });
	    if (traceValues) {
		switch (retval) {
		  case AssemblerState.VALUE:
		      TraceAcc(ilg, "after "+kind);
		      break;
		  case AssemblerState.NOVALUE:
		      TraceStrI(ilg, ">>> TRACE: no value after "+kind);
		      break;
		  default:
		      break;
		}
	    }
	}

	indent -= 2;
	return retval;
    }

    public void GenLoadLiteral(ILGenerator ilg, object literal) {
	// Short cut. Once bootstrapped, something more efficient should be done.
	GenLiteralCode(ilg, literal);
    }

    public AssemblerState Gen_lit(ILGenerator ilg, object node) {
	object val = NodeGet(node, "lit", "value");
	Trace(val);
	GenLoadLiteral(ilg, val);
	return AssemblerState.VALUE;
    }

    public AssemblerState Gen_singleton(ILGenerator ilg, object node) {
	string id = (string) NodeGet(node, "singleton", "identifier");
	Trace(id);
	if ("undefined".Equals(id)) {
	    ilg.Emit(OpCodes.Ldsfld, undefField);
	} else {
	    throw new Exception("Unsupported singleton identifier: "+id);
	}
	return AssemblerState.VALUE;
    }

    public class ArgInfo {
	public string name;
	public bool isCaptured;
	public bool isMutated;

	public ArgInfo(object ai) {
	    Pair arginfo = (Pair) ai;
	    name = (string) arginfo.Car;
	    isCaptured = (bool) arginfo.D.Car;
	    isMutated = (bool) arginfo.D.D.Car;
	}

	public override string ToString() {
	    return "ArgInfo("+name+"/c"+isCaptured+"/m"+isMutated+")";
	}
    };

    public AssemblerState Gen_var(ILGenerator ilg, object node) {
	object locObj = NodeGet(node, "var", "location");
	Trace(NodeGet(node, "var", "name"));
	bool isGlobal = (bool) NodeGet(node, "var", "global?");

	if (isGlobal) {
	    Trace("global location "+locObj);
	    FieldBuilder fB = RecordBinding((string) locObj);

	    GenLdClosure(ilg);
	    ilg.Emit(OpCodes.Ldfld, closureModule);
	    ilg.Emit(OpCodes.Ldfld, fB);
	    ilg.Emit(OpCodes.Ldfld, variableValue);

	    // Check for undefined.
	    Label lDefined = ilg.DefineLabel();
	    ilg.Emit(OpCodes.Dup);
	    ilg.Emit(OpCodes.Ldsfld, undefField);
	    ilg.Emit(OpCodes.Bne_Un, lDefined);

	    GenLdClosure(ilg);
	    ilg.Emit(OpCodes.Ldfld, closureModule);
	    ilg.Emit(OpCodes.Ldfld, fB);
	    ilg.Emit(OpCodes.Newobj, undefinedGlobal);
	    ilg.Emit(OpCodes.Throw);

	    ilg.MarkLabel(lDefined);

	} else {
	    ArgInfo ai = new ArgInfo(NodeGet(node, "var", "arginfo"));
	    int loc = (int) locObj;
	    bool isLocal = (loc < 0);

	    if (isLocal) {
		loc = -loc - 1;
		Trace("arg index "+loc+" arginfo "+ai);
		GenLdArgVec(ilg);
		ilg.Emit(OpCodes.Ldc_I4, loc);
		ilg.Emit(OpCodes.Ldelem_Ref);
	    } else {
		Trace("env index "+loc+" arginfo "+ai);
		GenLdClosure(ilg);
		ilg.Emit(OpCodes.Ldfld, currentEnv[loc]);
	    }

	    // If the variable is mutated AND captured, then the value
	    // just loaded is the cell, not the variable's value itself.
	    if (ai.isCaptured && ai.isMutated) {
		ilg.Emit(OpCodes.Ldfld, cellValue);
	    }
	}
	return AssemblerState.VALUE;
    }

    internal static AssemblerState requireValue(ILGenerator ilg,
						string where,
						bool noValueIsError,
						AssemblerState v)
    {
	switch (v) {
	  case AssemblerState.RETURNED:
	      throw new Exception("Returned during "+where+"!");

	  case AssemblerState.VALUE:
	      break;

	  case AssemblerState.NOVALUE:
	      if (noValueIsError) {
		  throw new Exception("No value for "+where+"!");
	      } else {
		  ilg.Emit(OpCodes.Ldsfld, undefField);
	      }
	      break;
	}

	return AssemblerState.VALUE;
    }

    public AssemblerState Gen_begin(ILGenerator ilg, object node) {
	switch (GenNode(ilg, NodeGet(node, "begin", "head"))) {
	  case AssemblerState.RETURNED:
	      throw new Exception("Returned from head of begin!");

	  case AssemblerState.VALUE:
	      ilg.Emit(OpCodes.Pop);
	      break;

	  case AssemblerState.NOVALUE:
	      break;
	}

	return GenNode(ilg, NodeGet(node, "begin", "tail"));
    }

    public AssemblerState Gen_lambda(ILGenerator ilg, object node) {
	ConstructorBuilder closureBuilder = GenClosure(node, false);
	GenLdClosure(ilg);
	GenLdArgVec(ilg);
	ilg.Emit(OpCodes.Newobj, closureBuilder);
	return AssemblerState.VALUE;
    }

    public AssemblerState Gen_apply(ILGenerator ilg, object node) {
	object rator = NodeGet(node, "apply", "rator");
	List rands = (List) NodeGet(node, "apply", "rands");
	int argc = rands.ListLength();
	LocalBuilder frame = getVecLocal(ilg);

	// Need the +1 just below in case callee is varargs and is
	// being passed no arguments that will be put in the
	// rest-list. If we omit the +1, there may be no slot to put
	// the built (empty) list. A concrete example:
	//
	// ((lambda (a b . x) x) 1 2 3)
	//    ; works without the +1 since there's at least
	//    ; one argument that will be put in the rest-list,
	//    ; so the list-building code has somewhere to
	//    ; put its result. See uses of "isVA" in GenClosure.
	// ((lambda (a b . x) x) 1 2)
	//    ; won't work without the +1 since the argument
	//    ; vector is of length 2, so there's no room to
	//    ; put the "built" empty-list in the third slot.
	//
	ilg.Emit(OpCodes.Ldc_I4, argc + 1);
	ilg.Emit(OpCodes.Newarr, typeof(System.Object));
	ilg.Emit(OpCodes.Stloc, frame);

	int counter = 0;
	foreach (object rand in rands) {
	    ilg.Emit(OpCodes.Ldloc, frame);
	    ilg.Emit(OpCodes.Ldc_I4, counter++);
	    requireValue(ilg, "arg "+counter, false, GenNode(ilg, rand));
	    ilg.Emit(OpCodes.Stelem_Ref);
	}
	GenLdParamAddr(ilg);
	ilg.Emit(OpCodes.Ldloc, frame);
	ilg.Emit(OpCodes.Stind_Ref);
	returnVecLocal(frame);
	requireValue(ilg, "rator", true, GenNode(ilg, rator));
	ilg.Emit(OpCodes.Ret);
	return AssemblerState.RETURNED;
    }

    private void TraceAcc(ILGenerator ilg, string where) {
	TraceAcc(ilg, where, typeof(System.Object));
    }

    private void TraceStrI(ILGenerator ilg, string x) {
	ilg.Emit(OpCodes.Ldstr, x);
	ilg.Emit(OpCodes.Call,
		 typeof(System.Console).GetMethod("WriteLine", new Type[1] {typeof(string)}));
    }

    private void TraceAcc(ILGenerator ilg, string where, Type t) {
	if (suppress1) {
	    ilg.Emit(OpCodes.Ldstr, ">>> TRACE: "+ where+ ": **SUPPRESSED**");
	    ilg.Emit(OpCodes.Call,
		     typeof(System.Console).GetMethod("WriteLine", new Type[1] {typeof(object)}));
	    suppress1 = false;
	} else {
	    ilg.Emit(OpCodes.Ldstr, ">>> TRACE: "+ where+ ": ");
	    ilg.Emit(OpCodes.Call,
		     typeof(System.Console).GetMethod("Write", new Type[1] {typeof(object)}));
	    ilg.Emit(OpCodes.Dup);
	    ilg.Emit(OpCodes.Call,
		     typeof(System.Console).GetMethod("Write", new Type[1] {t}));

	    ilg.Emit(OpCodes.Ldstr, " :: ");
	    ilg.Emit(OpCodes.Call,
		     typeof(System.Console).GetMethod("Write", new Type[1] {typeof(object)}));
	    ilg.Emit(OpCodes.Dup);
	    ilg.Emit(OpCodes.Callvirt,
		     typeof(System.Object).GetMethod("GetType", new Type[0] {}));
	    ilg.Emit(OpCodes.Call,
		     typeof(System.Console).GetMethod("WriteLine", new Type[1] {t}));
	}
    }

    public AssemblerState Gen_if(ILGenerator ilg, object node) {
	object eTest = NodeGet(node, "if", "test");
	object eTrue = NodeGet(node, "if", "true");
	object eFalse = NodeGet(node, "if", "false");

	Label trueLabelPopping = ilg.DefineLabel();
	Label trueLabel = ilg.DefineLabel();
	Label endLabel = ilg.DefineLabel();
	AssemblerState retval;

	requireValue(ilg, "test of if", false, GenNode(ilg, eTest));
	ilg.Emit(OpCodes.Isinst, typeof(System.Boolean));
	ilg.Emit(OpCodes.Dup);
	ilg.Emit(OpCodes.Brfalse, trueLabelPopping);
	ilg.Emit(OpCodes.Unbox, typeof(System.Boolean));
	ilg.Emit(OpCodes.Ldind_I1);
	ilg.Emit(OpCodes.Brtrue, trueLabel);

	AssemblerState sFalse = GenNode(ilg, eFalse);

	if (sFalse != AssemblerState.RETURNED) {
	    requireValue(ilg, "false branch of if", false, sFalse);
	    ilg.Emit(OpCodes.Br, endLabel);
	    retval = AssemblerState.VALUE;
	} else {
	    retval = AssemblerState.RETURNED;
	}

	ilg.MarkLabel(trueLabelPopping);
	ilg.Emit(OpCodes.Pop);
	ilg.MarkLabel(trueLabel);
	AssemblerState sTrue = GenNode(ilg, eTrue);

	if (sTrue != AssemblerState.RETURNED) {
	    requireValue(ilg, "true branch of if", false, sFalse);
	    if (retval != AssemblerState.VALUE)
		throw new Exception("True branch has value, but false branch returned");
	} else {
	    if (retval != AssemblerState.RETURNED)
		throw new Exception("True branch returned, but false branch has value");
	}

	ilg.MarkLabel(endLabel);
	return retval;
    }

    public AssemblerState Gen_set(ILGenerator ilg, object node) {
	object locObj = NodeGet(node, "set", "location");
	object eValue = NodeGet(node, "set", "value");
	bool isGlobal = locObj is string;

	if (isGlobal) {
	    FieldBuilder fB = RecordBinding((string) locObj);
	    GenLdClosure(ilg);
	    ilg.Emit(OpCodes.Ldfld, closureModule);
	    ilg.Emit(OpCodes.Ldfld, fB);
	    requireValue(ilg, "value of global set", false, GenNode(ilg, eValue));
	    ilg.Emit(OpCodes.Stfld, variableValue);
	} else {
	    ArgInfo ai = new ArgInfo(NodeGet(node, "set", "arginfo"));
	    int loc = (int) locObj;
	    bool isLocal = (loc < 0);

	    if (!ai.isMutated) {
		throw new Exception("Attempt to set non-mutated arginfo: "+ai);
	    }

	    if (isLocal) {
		loc = -loc - 1;
		GenLdArgVec(ilg);
		ilg.Emit(OpCodes.Ldc_I4, loc);
		if (ai.isCaptured) {
		    // Both captured AND mutated.
		    ilg.Emit(OpCodes.Ldelem_Ref);
		    requireValue(ilg, "value of local set (cap)", false, GenNode(ilg, eValue));
		    ilg.Emit(OpCodes.Stfld, cellValue);
		} else {
		    requireValue(ilg, "value of local set (uncap)", false, GenNode(ilg, eValue));
		    ilg.Emit(OpCodes.Stelem_Ref);
		}
	    } else {
		GenLdClosure(ilg);
		// : envvec/argvec elementIndex
		if (ai.isCaptured) {
		    // We're both captured AND mutated, then. That means
		    // we should load the cell from our computed location,
		    // and update the value stored within.
		    ilg.Emit(OpCodes.Ldfld, currentEnv[loc]);
		    requireValue(ilg, "value of local set (cap)", false, GenNode(ilg, eValue));
		    ilg.Emit(OpCodes.Stfld, cellValue);
		} else {
		    requireValue(ilg, "value of local set (uncap)", false, GenNode(ilg, eValue));
		    ilg.Emit(OpCodes.Stfld, currentEnv[loc]);
		}
	    }
	}
	return AssemblerState.NOVALUE;
    }

    public bool ListContainsSym(List l, string s) {
	foreach (object o in l) {
	    if (o is string && ((string) o).Equals(s))
		return true;
	}
	return false;
    }

    public AssemblerState Gen_extern_apply(ILGenerator ilg, object node) {
	Pair detail = (Pair) NodeGet(node, "extern-apply", "detail");
	string kind = ((string) detail.Car).Replace('-', '_');
	detail = detail.D;

	AssemblerState retval;

	Trace(kind);
	Trace(detail);
	MethodInfo mi = typeof(Sil).GetMethod("GenExtApp_"+kind,
					      BindingFlags.NonPublic |
					      BindingFlags.Instance,
					      null,
					      new Type[3] {
						  typeof(ILGenerator),
						  typeof(Pair),
						  typeof(Pair)
					      },
					      null);
	if (mi == null) {
	    throw new Exception("Unsupported extern-apply kind: "+kind);
	} else {
	    retval = (AssemblerState) mi.Invoke(this, new Object[3] { ilg, node, detail });
	}

	return retval;
    }

    private string ExtTypeName(object a) {
	if (a is string) {
	    string s = (string) a;
	    if ("object".Equals(s)) { return "System.Object"; }
	    if ("symbol".Equals(s)) { return "System.String"; }
	    if ("string".Equals(s)) { return "Newmoon.SchemeString"; }
	    if ("int".Equals(s)) { return "System.Int32"; }
	    if ("pair".Equals(s)) { return "Newmoon.Pair"; }
	    if ("list".Equals(s)) { return "Newmoon.List"; }
	    if ("closure".Equals(s)) { return "Newmoon.Closure"; }
	    if ("char".Equals(s)) { return "System.Char"; }
	    if ("vector".Equals(s)) { return "System.Object[]"; }
	    if ("bool".Equals(s)) { return "System.Boolean"; }
	    if ("input-port".Equals(s)) { return "System.IO.TextReader"; }
	    if ("output-port".Equals(s)) { return "System.IO.TextWriter"; }
	    throw new Exception("Unknown external-type symbol: "+s);
	}
	if (!(a is SchemeString))
	    throw new Exception("External-type must be string or symbol: "+a);
	return ((SchemeString) a).ToString();
    }

    private string[] ExtTypeNames(List l) {
	string[] res = new string[l.ListLength()];
	int counter = 0;
	foreach (object o in l) {
	    res[counter++] = ExtTypeName(o);
	}
	return res;
    }

    private Type lookupType(string typeName) {
	Type res = Type.GetType(typeName);
	if (res != null)
	    return res;
	foreach (Assembly a in assemblies) {
	    //System.Console.WriteLine("Searching assembly "+a+" for type "+typeName+"...");
	    res = a.GetType(typeName);
	    if (res != null) {
		//System.Console.WriteLine("Found: "+res);
		return res;
	    }
	}
	return null;
    }

    private Type[] ExtTypes(string[] typeNames) {
	Type[] res = new Type[typeNames.Length];
	for (int i = 0; i < typeNames.Length; i++) {
	    res[i] = lookupType(typeNames[i]);
	    if (res[i] == null)
		throw new Exception("Unknown type: "+typeNames[i]);
	}
	return res;
    }

    private AssemblerState resultGuard(ILGenerator ilg, MethodInfo mi) {
	if (mi.ReturnType == voidType) {
	    Trace("rettype "+mi.ReturnType+" - that's NOVALUE");
	    return AssemblerState.NOVALUE;
	} else {
	    if (mi.ReturnType.IsValueType) {
		Trace("rettype "+mi.ReturnType+" - valuetype - that's VALUE");
		ilg.Emit(OpCodes.Box, mi.ReturnType);
	    } else {
		Trace("rettype "+mi.ReturnType+" - nonvaluetype - that's VALUE");
	    }
	    return AssemblerState.VALUE;
	}
    }

    public void PushDotNetArg(ILGenerator ilg, object node, Type t, int pos) {
	requireValue(ilg, "arg "+pos, false, GenNode(ilg, node));
	if (t != null && t != typeof(Object)) {
	    if (t.IsValueType) {
		ilg.Emit(OpCodes.Unbox, t);
		ilg.Emit(OpCodes.Ldobj, t);
	    } else {
		ilg.Emit(OpCodes.Castclass, t);
	    }
	}
    }

    public int PushDotNetArgs(ILGenerator ilg, List rands, Type[] types, int firstPos) {
	int pos = firstPos;
	int counter = 0;
	foreach (object node in rands) {
	    PushDotNetArg(ilg, node, (types == null) ? null : types[counter], pos);
	    pos++;
	    counter++;
	}
	return pos;
    }

    internal AssemblerState GenExtApp_specific_static(ILGenerator ilg, Pair node, Pair detail) {
	List rands = (List) NodeGet(node, "extern-apply", "rands");
	int argc = rands.ListLength();

	List flags = (List) detail.Car; detail = detail.D;
	string name = (string) detail.Car; detail = detail.D;
	Pair rest = detail.A;
	string theTypeName = ExtTypeName(rest.Car); rest = rest.D;
	Type theType = lookupType(theTypeName);
	string methodName = ((SchemeString) rest.Car).ToString();
	Type[] paramTypes = ExtTypes(ExtTypeNames((List) rest.Cdr));

	if (argc != paramTypes.Length)
	    throw new Exception("Argument count mismatch in specific-static: "+
				theTypeName+"."+methodName);

	if (theType == null)
	    throw new Exception("Unknown type in specific-static: "+
				theTypeName+"."+methodName);

	MethodInfo mi = theType.GetMethod(methodName,
					  BindingFlags.Public |
					  BindingFlags.Static,
					  null,
					  paramTypes,
					  null);
	if (mi == null)
	    throw new Exception("Specific-static not found: "+theType+"."+methodName);

	PushDotNetArgs(ilg, rands, paramTypes, 0);
	ilg.Emit(OpCodes.Call, mi);
	return resultGuard(ilg, mi);
    }

    internal AssemblerState GenExtApp_generic_static(ILGenerator ilg, Pair node, Pair detail) {
	return _GenExtApp_gen_or_var_static(ilg, node, detail, false);
    }

    internal AssemblerState GenExtApp_varargs_static(ILGenerator ilg, Pair node, Pair detail) {
	return _GenExtApp_gen_or_var_static(ilg, node, detail, true);
    }

    internal AssemblerState _GenExtApp_gen_or_var_static(ILGenerator ilg,
						       Pair node,
						       Pair detail,
						       bool isVarargs)
    {
	List flags = (List) detail.Car; detail = detail.D;
	string name = (string) detail.Car; detail = detail.D;
	Pair rest = detail.A;
	string typeName = ExtTypeName(rest.Car);
	string methodName = ((SchemeString) rest.D.Car).ToString();

	if (lookupType(typeName) == null)
	    throw new Exception("Unknown type in generic/varargs-static: "+
				typeName+"."+methodName);

	List rands = (List) NodeGet(node, "extern-apply", "rands");
	int argc = rands.ListLength();

	LocalBuilder a = getVecLocal(ilg);
	ilg.Emit(OpCodes.Ldc_I4, argc);
	ilg.Emit(OpCodes.Newarr, typeof(System.Object));
	ilg.Emit(OpCodes.Stloc, a);
	int counter = 0;
	foreach (Pair rand in rands) {
	    ilg.Emit(OpCodes.Ldloc, a);
	    ilg.Emit(OpCodes.Ldc_I4, counter++);
	    requireValue(ilg, "arg "+counter, false, GenNode(ilg, rand));
	    ilg.Emit(OpCodes.Stelem_Ref);
	}

	GenLdClosure(ilg);			// requesting-closure
	ilg.Emit(OpCodes.Ldstr, typeName);	// typename
	ilg.Emit(OpCodes.Ldstr, methodName);	// methodname
	ilg.Emit(OpCodes.Ldloc, a);		// argvec
	returnVecLocal(a);
	if (isVarargs) {
	    ilg.Emit(OpCodes.Call, environmentInvokeVarargsStatic);
	} else {
	    ilg.Emit(OpCodes.Call, environmentInvokeGenericStatic);
	}
	return AssemblerState.VALUE;
    }

    internal AssemblerState GenExtApp_type_predicate(ILGenerator ilg, Pair node, Pair detail) {
	List flags = (List) detail.Car; detail = detail.D;
	string name = (string) detail.Car; detail = detail.D;
	Pair rest = detail.A;
	string typeName = ExtTypeName(rest.Car);
	Type theType = lookupType(typeName);

	List rands = (List) NodeGet(node, "extern-apply", "rands");
	int argc = rands.ListLength();

	if (theType == null)
	    throw new Exception("Type not found for type-predicate: "+typeName);

	if (argc != 1)
	    throw new Exception("Wrong argc to type-predicate: "+argc+" to "+typeName);

	requireValue(ilg, "argument of type-predicate", false, GenNode(ilg, ((Pair) rands).Car));
	ilg.Emit(OpCodes.Isinst, theType);
	ilg.Emit(OpCodes.Ldnull);
	ilg.Emit(OpCodes.Ceq);
	ilg.Emit(OpCodes.Ldc_I4_1);
	ilg.Emit(OpCodes.Xor);
	ilg.Emit(OpCodes.Box, typeof(Boolean));
	return AssemblerState.VALUE;
    }

    internal AssemblerState GenExtApp_specific_constructor(ILGenerator ilg, Pair node, Pair detail) {
	List rands = (List) NodeGet(node, "extern-apply", "rands");
	int argc = rands.ListLength();

	List flags = (List) detail.Car; detail = detail.D;
	string name = (string) detail.Car; detail = detail.D;
	Pair rest = detail.A;
	string theTypeName = ExtTypeName(rest.Car);
	Type theType = lookupType(theTypeName);
	Type[] paramTypes = ExtTypes(ExtTypeNames((List) rest.Cdr));

	if (argc != paramTypes.Length)
	    throw new Exception("Argument count mismatch in constructor: "+theTypeName);

	if (theType == null)
	    throw new Exception("Unknown type in constructor: "+theTypeName);

	ConstructorInfo ci = theType.GetConstructor(paramTypes);
	if (ci == null)
	    throw new Exception("Constructor not found for type: "+theType);

	PushDotNetArgs(ilg, rands, paramTypes, 0);
	ilg.Emit(OpCodes.Newobj, ci);
	return AssemblerState.VALUE;
    }

    internal AssemblerState GenExtApp_closure_constructor(ILGenerator ilg, Pair node, Pair detail) {
	List rands = (List) NodeGet(node, "extern-apply", "rands");
	int argc = rands.ListLength();

	List flags = (List) detail.Car; detail = detail.D;
	string name = (string) detail.Car; detail = detail.D;
	Pair rest = detail.A;
	string theTypeName = ExtTypeName(rest.Car);
	Type theType = lookupType(theTypeName);
	Type[] paramTypes = ExtTypes(ExtTypeNames(new Pair(new SchemeString("Newmoon.Module"),
							   (List) rest.Cdr)));

	if (argc != paramTypes.Length - 1)
	    throw new Exception("Argument count mismatch in closure constructor: "+theTypeName);

	if (theType == null)
	    throw new Exception("Unknown type in constructor: "+theTypeName);

	ConstructorInfo ci = theType.GetConstructor(paramTypes);
	if (ci == null)
	    throw new Exception("Constructor not found for type: "+theType);

	GenLdClosure(ilg);
	ilg.Emit(OpCodes.Ldfld, closureModule);
	PushDotNetArgs(ilg, rands, paramTypes, 0);
	ilg.Emit(OpCodes.Newobj, ci);
	return AssemblerState.VALUE;
    }

    internal AssemblerState GenExtApp_generic_constructor(ILGenerator ilg, Pair node, Pair detail) {
	throw new NotImplementedException();
	//return AssemblerState.VALUE;
    }

    internal void takeValueTypeAddress(ILGenerator ilg, Type theType) {
	if (theType.IsValueType) {
	    LocalBuilder lb = ilg.DeclareLocal(theType);
	    ilg.Emit(OpCodes.Stloc, lb);
	    ilg.Emit(OpCodes.Ldloca, lb);
	}
    }

    internal AssemblerState GenExtApp_specific_property(ILGenerator ilg, Pair node, Pair detail) {
	List rands = (List) NodeGet(node, "extern-apply", "rands");
	int argc = rands.ListLength();

	List flags = (List) detail.Car; detail = detail.D;
	bool isReader = ListContainsSym(flags, "reader");
	bool isWriter = ListContainsSym(flags, "writer");

	string name = (string) detail.Car; detail = detail.D;
	Pair rest = detail.A;
	string typeName = ExtTypeName(rest.Car);
	Type theType = lookupType(typeName);
	string propertyName = ((SchemeString) rest.D.Car).ToString();

	string[] argTypeNames = ExtTypeNames((List) rest.D.Cdr);
	Type[] argTypes = ExtTypes(argTypeNames);
	int expectedArgc = argTypes.Length;

	if (isReader && isWriter)
	    throw new Exception("Can't be both reader and writer of property: "+
				typeName+"."+propertyName);

	if (!(isReader || isWriter))
	    throw new Exception("Can't be neither reader nor writer of property: "+
				typeName+"."+propertyName);

	if (theType == null)
	    throw new Exception("Unknown type for property: "+
				typeName+"."+propertyName);

	PropertyInfo pi = theType.GetProperty(propertyName, argTypes);
	if (pi == null)
	    throw new Exception("Property "+propertyName+" not found for type: "+
				theType+" with "+expectedArgc+" parameters");

	if (isReader) {
	    if (argc != (expectedArgc + 1))
		throw new Exception("Property reader expects exactly "+
				    (expectedArgc+1)+" argument(s): "+
				    typeName+"."+propertyName);
	    if (!pi.CanRead)
		throw new Exception("Property is unreadable: "+typeName+"."+propertyName);

	    PushDotNetArg(ilg, ((Pair) rands).Car, theType, 0);
	    takeValueTypeAddress(ilg, theType);
	    PushDotNetArgs(ilg, (List) ((Pair) rands).Cdr, argTypes, 1);
	    MethodInfo mi = pi.GetGetMethod();
	    ilg.Emit(theType.IsValueType ? OpCodes.Call : OpCodes.Callvirt, mi);
	    return resultGuard(ilg, mi);
	} else {
	    if (argc != (expectedArgc + 2))
		throw new Exception("Property writer expects exactly "+
				    (expectedArgc+2)+" arguments: "+
				    typeName+"."+propertyName);
	    if (!pi.CanWrite)
		throw new Exception("Property is unwritable: "+typeName+"."+propertyName);

	    PushDotNetArg(ilg, ((Pair) rands).Car, theType, 0);
	    takeValueTypeAddress(ilg, theType);

	    Pair r = ((Pair) rands).D;
	    Type[] restArgs = new Type[expectedArgc + 1];
	    System.Array.Copy(argTypes, 0, restArgs, 0, argTypes.Length);
	    restArgs[expectedArgc] = pi.PropertyType;
	    PushDotNetArgs(ilg, r, restArgs, 1);

	    MethodInfo mi = pi.GetSetMethod();
	    ilg.Emit(theType.IsValueType ? OpCodes.Call : OpCodes.Callvirt, mi);
	    return resultGuard(ilg, mi);
	}
    }

    internal AssemblerState GenExtApp_generic_property(ILGenerator ilg, Pair node, Pair detail) {
	throw new NotImplementedException();
    }

    internal AssemblerState GenExtApp_specific_method(ILGenerator ilg, Pair node, Pair detail) {
	List rands = (List) NodeGet(node, "extern-apply", "rands");
	int argc = rands.ListLength();

	List flags = (List) detail.Car; detail = detail.D;
	string name = (string) detail.Car; detail = detail.D;
	Pair rest = detail.A;
	string theTypeName = ExtTypeName(rest.Car);
	Type theType = lookupType(theTypeName);
	string methodName = ((SchemeString) rest.D.Car).ToString();
	Type[] paramTypes = ExtTypes(ExtTypeNames((List) rest.D.Cdr));

	if (theType == null)
	    throw new Exception("Unknown type in method "+methodName+" of "+theTypeName);

	if (argc != (paramTypes.Length + 1))
	    throw new Exception("Argument count mismatch in method "+methodName+" of "+theType);

	MethodInfo mi = theType.GetMethod(methodName, paramTypes);
	if (mi == null)
	    throw new Exception("Method not found for method "+methodName+" of "+theType);

	PushDotNetArg(ilg, ((Pair) rands).Car, theType, 0);
	takeValueTypeAddress(ilg, theType);

	rands = (List) ((Pair) rands).Cdr;
	PushDotNetArgs(ilg, rands, paramTypes, 1);

	ilg.Emit(theType.IsValueType ? OpCodes.Call : OpCodes.Callvirt, mi);
	return resultGuard(ilg, mi);
    }

    internal AssemblerState GenExtApp_overloaded_method(ILGenerator ilg, Pair node, Pair detail) {
	throw new NotImplementedException();
    }

    internal AssemblerState GenExtApp_generic_method(ILGenerator ilg, Pair node, Pair detail) {
	List flags = (List) detail.Car; detail = detail.D;
	string name = (string) detail.Car; detail = detail.D;
	Pair rest = detail.A;
	string methodName = ((SchemeString) rest.Car).ToString();

	List rands = (List) NodeGet(node, "extern-apply", "rands");
	int argc = rands.ListLength();

	if (argc < 1)
	    throw new Exception("generic-method call "+methodName+" needs at least one argument");

	LocalBuilder a = getVecLocal(ilg);
	ilg.Emit(OpCodes.Ldc_I4, argc);
	ilg.Emit(OpCodes.Newarr, typeof(System.Object));
	ilg.Emit(OpCodes.Stloc, a);
	int counter = 0;
	foreach (Pair rand in rands) {
	    ilg.Emit(OpCodes.Ldloc, a);
	    ilg.Emit(OpCodes.Ldc_I4, counter++);
	    requireValue(ilg, "arg "+counter, false, GenNode(ilg, rand));
	    ilg.Emit(OpCodes.Stelem_Ref);
	}

	GenLdClosure(ilg);			// requesting-closure
	ilg.Emit(OpCodes.Ldstr, methodName);	// methodname
	ilg.Emit(OpCodes.Ldloc, a);		// argvec
	returnVecLocal(a);
	ilg.Emit(OpCodes.Call, environmentInvokeGenericMethod);
	return AssemblerState.VALUE;
    }

    internal AssemblerState GenExtApp_static_variable(ILGenerator ilg, Pair node, Pair detail) {
	List rands = (List) NodeGet(node, "extern-apply", "rands");
	int argc = rands.ListLength();

	List flags = (List) detail.Car; detail = detail.D;
	string name = (string) detail.Car; detail = detail.D;
	Pair rest = detail.A;
	string theTypeName = ExtTypeName(rest.Car);
	Type theType = lookupType(theTypeName);
	string varName = ((SchemeString) rest.D.Car).ToString();

	if (theType == null)
	    throw new Exception("Unknown type in static-variable "+varName+" of "+theTypeName);

	if (argc != 1 && argc != 0)
	    throw new Exception("Need 0 or 1 arguments to static field "+varName+" of "+theType);

	FieldInfo fi = theType.GetField(varName,
					BindingFlags.Public |
					BindingFlags.Static);
	if (fi == null)
	    throw new Exception("Static field not found for "+varName+" of "+theType);

	if (argc == 0) {
	    Trace("ldsfld "+theType+"."+varName);
	    ilg.Emit(OpCodes.Ldsfld, fi);
	    return AssemblerState.VALUE;
	} else {
	    Trace("stsfld "+theType+"."+varName);
	    PushDotNetArg(ilg, ((Pair) rands).Car, fi.FieldType, 0);
	    ilg.Emit(OpCodes.Stsfld, fi);
	    return AssemblerState.NOVALUE;
	}
    }

    internal AssemblerState GenExtApp_assembler(ILGenerator ilg, Pair node, Pair detail) {
	List flags = (List) detail.Car; detail = detail.D;
	string name = (string) detail.Car; detail = detail.D;
	Pair rest = detail.A;
	string typeName = ExtTypeName(rest.Car);
	Type t = lookupType(typeName);

	if (t == null)
	    throw new Exception("Unknown type in assembler: "+typeName);

	System.Console.WriteLine(t);
	System.Console.WriteLine(t.GetConstructor(new Type[0] {}).Invoke(null));
	IAssembler ia = (IAssembler) t.GetConstructor(new Type[0] {}).Invoke(null);
	return ia.Assemble(ilg, this, node, flags, name, (List) rest.Cdr);
    }

    private int lambdaCount = 1;
    private ConstructorBuilder GenClosure(object node, bool isRootLambda) {
	List captures = NodeGetOrEmpty(node, "lambda", "captures");
	bool isVA = (bool) NodeGetOrFalse(node, "lambda", "varargs");
	List arglist = NodeGetOrEmpty(node, "lambda", "args");
	object body = NodeGet(node, "lambda", "body");

	string lambdaName = prefixed("lambda"+(lambdaCount++));
	Trace("(start of "+lambdaName+")");

	TypeBuilder parentLambda = currentLambda;
	currentLambda = moduleBuilder.DefineType(lambdaName,
						 TypeAttributes.NotPublic,
						 typeof(Newmoon.Closure));

	Type[] cTypes;
	if (isRootLambda)
	    cTypes = new Type[1] { entryPoint };
	else
	    cTypes = new Type[2] { parentLambda, typeof(System.Object[]) };

	ConstructorBuilder cB = currentLambda.DefineConstructor(MethodAttributes.Public |
								MethodAttributes.HideBySig,
								CallingConventions.Standard,
								cTypes);

	ILGenerator ilg = cB.GetILGenerator();
	List oldLocals = pushVecLocals();

	// Call superclass constructor with our module.
	//
	ConstructorInfo cInfo =
	    typeof(Newmoon.Closure).GetConstructor(BindingFlags.NonPublic |
						       BindingFlags.Instance,
						       null,
						       new Type[1] {
							   typeof(Newmoon.Module)
						       },
						       null);

	ilg.Emit(OpCodes.Ldarg_0);			// : this
	if (isRootLambda) {
	    ilg.Emit(OpCodes.Ldarg_1);			// : this module
	} else {
	    ilg.Emit(OpCodes.Ldarg_1);			// : this parent
	    ilg.Emit(OpCodes.Ldfld, closureModule);	// : this module
	}
	ilg.Emit(OpCodes.Call, cInfo);

	// Capture values from our parent's environment and arguments.
	//

	// (NOTE: Must restore currentEnv later in the method!)
	FieldBuilder[] parentEnv = currentEnv;
	currentEnv = new FieldBuilder[captures.ListLength()];

	if (isRootLambda && captures != Null.NULL)
	    throw new Exception("Root lambda must have no captures");

	foreach (Pair capture in captures) {
	    int newloc = (int) capture.D.Car;
	    int oldloc = (int) capture.D.D.Car;

	    string fName = "envt_"+((string)capture.A.Car)+"_"+newloc;
	    currentEnv[newloc] = currentLambda.DefineField(fName, typeof(object),
							   FieldAttributes.Assembly);

	    ilg.Emit(OpCodes.Ldarg_0);				// : this
	    if (oldloc < 0) {
		ilg.Emit(OpCodes.Ldarg_2);			// : this parent-argvec
		ilg.Emit(OpCodes.Ldc_I4, -oldloc - 1);		// : this parent-argvec oldloc
		ilg.Emit(OpCodes.Ldelem_Ref);			// : this parent-argval
	    } else {
		ilg.Emit(OpCodes.Ldarg_1);			// : this parent
		ilg.Emit(OpCodes.Ldfld, parentEnv[oldloc]);	// : this parent-envval
	    }
	    ilg.Emit(OpCodes.Stfld, currentEnv[newloc]);	// :
	}

	// Return from constructor.
	//
	ilg.Emit(OpCodes.Ret);
	popVecLocals(oldLocals);

	MethodBuilder mB =
	    currentLambda.DefineMethod("Apply",
				       MethodAttributes.Public |
				       MethodAttributes.Virtual |
				       MethodAttributes.HideBySig,
				       typeof(Newmoon.Closure),
				       new Type[2] {
					   typeof(System.Object[]),
					   // %%% NOTE: pnet: Type.GetType leaks
					   // for refs and ptrs! Fix it later
					   // (use "synthetic types")
					   Type.GetType("System.Object[]&")
				       });
	ilg = mB.GetILGenerator();
	oldLocals = pushVecLocals();

	// Check argc and build rest-list.
	//
	if (isVA) {
	    int minargc = arglist.ListLength() - 1;
	    GenLdArgVec(ilg);
	    ilg.Emit(OpCodes.Ldc_I4, minargc + 1); // count the varargs slot as a required arg
	    ilg.Emit(OpCodes.Call, closureCheckVarArgc);

	    LocalBuilder vIndex = ilg.DeclareLocal(typeof(int));
	    LocalBuilder vList = ilg.DeclareLocal(typeof(Newmoon.List));
	    GenLdArgVec(ilg);
	    ilg.Emit(OpCodes.Ldlen);
	    ilg.Emit(OpCodes.Ldc_I4_2); // skip the final element because of empty-varargs problem
	    ilg.Emit(OpCodes.Sub);
	    ilg.Emit(OpCodes.Stloc, vIndex);
	    ilg.Emit(OpCodes.Ldsfld, nullNullField);
	    ilg.Emit(OpCodes.Stloc, vList);

	    Label loopTop = ilg.DefineLabel();
	    Label loopBreak = ilg.DefineLabel();

	    ilg.MarkLabel(loopTop);
	    ilg.Emit(OpCodes.Ldloc, vIndex);
	    ilg.Emit(OpCodes.Ldc_I4, minargc);
	    ilg.Emit(OpCodes.Blt, loopBreak);

	    GenLdArgVec(ilg);
	    ilg.Emit(OpCodes.Ldloc, vIndex);
	    ilg.Emit(OpCodes.Ldelem_Ref);
	    ilg.Emit(OpCodes.Ldloc, vList);
	    ilg.Emit(OpCodes.Newobj, mutableCons);
	    ilg.Emit(OpCodes.Stloc, vList);
	    ilg.Emit(OpCodes.Ldloc, vIndex);
	    ilg.Emit(OpCodes.Ldc_I4_1);
	    ilg.Emit(OpCodes.Sub);
	    ilg.Emit(OpCodes.Stloc, vIndex);
	    ilg.Emit(OpCodes.Br, loopTop);
	    ilg.MarkLabel(loopBreak);

	    GenLdArgVec(ilg);
	    ilg.Emit(OpCodes.Ldc_I4, minargc);
	    ilg.Emit(OpCodes.Ldloc, vList);
	    ilg.Emit(OpCodes.Stelem_Ref);
	} else {
	    int argc = arglist.ListLength();
	    GenLdArgVec(ilg);
	    ilg.Emit(OpCodes.Ldc_I4, argc + 1); // count the varargs slot as a required arg
	    ilg.Emit(OpCodes.Call, closureCheckArgc);
	}

	// Now build cells for argument variables that are both
	// captured and mutated.
	//
	int counter = 0;
	foreach (Pair arg in arglist) {
	    ArgInfo ai = new ArgInfo(arg);
	    if (ai.isCaptured && ai.isMutated) {
		// :

		GenLdArgVec(ilg);
		ilg.Emit(OpCodes.Ldc_I4, counter);
		// : argvec counter

		GenLdArgVec(ilg);
		ilg.Emit(OpCodes.Ldc_I4, counter);
		ilg.Emit(OpCodes.Ldelem_Ref);
		// : argvec counter argval

		ilg.Emit(OpCodes.Newobj, cellConstructor);
		// : argvec counter cell

		ilg.Emit(OpCodes.Stelem_Ref);
		// :
	    }
	    counter++;
	}

	// Main body.
	//
	if (AssemblerState.RETURNED != GenNode(ilg, body)) {
	    throw new Exception("Lambda "+lambdaName+" body didn't return!");
	}

	popVecLocals(oldLocals);
	Trace("(end-of-body)");

	// Restore currentEnv.
	currentEnv = parentEnv;

	currentLambda.CreateType();
	// Restore currentLambda.
	currentLambda = parentLambda;

	return cB;
    }
}

public class EqAssembler: Newmoon.IAssembler {
    public AssemblerState Assemble(ILGenerator ilg,
				   ICodeGenerator cg,
				   Pair node,
				   List flags,
				   string name,
				   List args)
    {
	List rands = (List) cg.NodeGet(node, "extern-apply", "rands");
	int argc = rands.ListLength();

	if (argc != 2) {
	    throw new Exception("eq? needs exactly two arguments, not "+
				argc);
	}

	object opA = ((Pair) rands).Car;
	object opB = ((Pair) rands).D.Car;

	cg.PushDotNetArg(ilg, opA, null, 0);
	cg.PushDotNetArg(ilg, opB, null, 0);
	ilg.Emit(OpCodes.Ceq);
	ilg.Emit(OpCodes.Box, typeof(System.Boolean));
	return AssemblerState.VALUE;
    }
}

public class ReboxAssembler: Newmoon.IAssembler {
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
	    throw new Exception("eq? needs exactly two arguments, not "+
				argc);
	}

	object opA = ((Pair) rands).Car;
	object opB = ((Pair) rands).D.Car;
	object opC = ((Pair) rands).D.D.Car;

	string tn1 = cg.NodeGet(opA, "lit", "value").ToString();
	string tn2 = cg.NodeGet(opB, "lit", "value").ToString();

	cg.PushDotNetArg(ilg, opC, Type.GetType(tn1), 2);
	ilg.Emit(OpCodes.Box, Type.GetType(tn2));
	return AssemblerState.VALUE;
    }
}

public abstract class IntOpAssembler: Newmoon.IAssembler {
    protected abstract string getName();
    protected abstract OpCode getOpCode();
    protected abstract Type getResultBox();

    public AssemblerState Assemble(ILGenerator ilg,
				   ICodeGenerator cg,
				   Pair node,
				   List flags,
				   string name,
				   List args)
    {
	List rands = (List) cg.NodeGet(node, "extern-apply", "rands");
	int argc = rands.ListLength();

	if (argc != 2) {
	    throw new Exception(getName()+" needs exactly two arguments, not "+
				argc);
	}

	object opA = ((Pair) rands).Car;
	object opB = ((Pair) rands).D.Car;

	cg.PushDotNetArg(ilg, opA, typeof(int), 0);
	cg.PushDotNetArg(ilg, opB, typeof(int), 1);
	ilg.Emit(getOpCode());
	Type r = getResultBox();
	if (r != null)
	    ilg.Emit(OpCodes.Box, r);
	return AssemblerState.VALUE;
    }
}

public class IntEqAssembler: IntOpAssembler {
    protected override string getName() { return "fx="; }
    protected override OpCode getOpCode() { return OpCodes.Ceq; }
    protected override Type getResultBox() { return typeof(System.Boolean); }
}

public class IntLtAssembler: IntOpAssembler {
    protected override string getName() { return "fx<"; }
    protected override OpCode getOpCode() { return OpCodes.Clt; }
    protected override Type getResultBox() { return typeof(System.Boolean); }
}

public class IntGtAssembler: IntOpAssembler {
    protected override string getName() { return "fx>"; }
    protected override OpCode getOpCode() { return OpCodes.Cgt; }
    protected override Type getResultBox() { return typeof(System.Boolean); }
}

public class IntPlusAssembler: IntOpAssembler {
    protected override string getName() { return "fx+"; }
    protected override OpCode getOpCode() { return OpCodes.Add; }
    protected override Type getResultBox() { return typeof(int); }
}

public class IntMinusAssembler: IntOpAssembler {
    protected override string getName() { return "fx-"; }
    protected override OpCode getOpCode() { return OpCodes.Sub; }
    protected override Type getResultBox() { return typeof(int); }
}

public class VectorRefAssembler: Newmoon.IAssembler {
    public AssemblerState Assemble(ILGenerator ilg,
				   ICodeGenerator cg,
				   Pair node,
				   List flags,
				   string name,
				   List args)
    {
	List rands = (List) cg.NodeGet(node, "extern-apply", "rands");
	int argc = rands.ListLength();

	if (argc != 2) {
	    throw new Exception("vector-ref needs exactly two arguments, not "+
				argc);
	}

	object opA = ((Pair) rands).Car;
	object opB = ((Pair) rands).D.Car;

	cg.PushDotNetArg(ilg, opA, typeof(System.Object[]), 0);
	cg.PushDotNetArg(ilg, opB, typeof(int), 1);
	ilg.Emit(OpCodes.Ldelem_Ref);
	return AssemblerState.VALUE;
    }
}

public class VectorSetAssembler: Newmoon.IAssembler {
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
	    throw new Exception("vector-set! needs exactly three arguments, not "+
				argc);
	}

	object opA = ((Pair) rands).Car;
	object opB = ((Pair) rands).D.Car;
	object opC = ((Pair) rands).D.D.Car;

	cg.PushDotNetArg(ilg, opA, typeof(System.Object[]), 0);
	cg.PushDotNetArg(ilg, opB, typeof(int), 1);
	cg.PushDotNetArg(ilg, opC, null, 2);
	ilg.Emit(OpCodes.Stelem_Ref);
	return AssemblerState.NOVALUE;
    }
}

public class VectorLengthAssembler: Newmoon.IAssembler {
    public AssemblerState Assemble(ILGenerator ilg,
				   ICodeGenerator cg,
				   Pair node,
				   List flags,
				   string name,
				   List args)
    {
	List rands = (List) cg.NodeGet(node, "extern-apply", "rands");
	int argc = rands.ListLength();

	if (argc != 1) {
	    throw new Exception("vector-length needs exactly one argument, not "+
				argc);
	}

	object opA = ((Pair) rands).Car;

	cg.PushDotNetArg(ilg, opA, typeof(System.Object[]), 0);
	ilg.Emit(OpCodes.Ldlen);
	ilg.Emit(OpCodes.Box, typeof(System.Int32));
	return AssemblerState.VALUE;
    }
}

public class LdnullAssembler: Newmoon.IAssembler {
    public AssemblerState Assemble(ILGenerator ilg,
				   ICodeGenerator cg,
				   Pair node,
				   List flags,
				   string name,
				   List args)
    {
	ilg.Emit(OpCodes.Ldnull);
	Sil.suppress1 = true;
	return AssemblerState.VALUE;
    }
}

namespace Newmoon {
    public class MakeVectorAssembler: Newmoon.IAssembler {
	public AssemblerState Assemble(ILGenerator ilg,
				       ICodeGenerator cg,
				       Pair node,
				       List flags,
				       string name,
				       List args)
	{
	    List rands = (List) cg.NodeGet(node, "extern-apply", "rands");
	    int argc = rands.ListLength();

	    switch (argc) {
	      case 1: {
		  object opA = ((Pair) rands).Car;
		  cg.PushDotNetArg(ilg, opA, typeof(int), 0);
		  ilg.Emit(OpCodes.Newarr, typeof(System.Object));
		  break;
	      }

	      case 2: {
		  object opA = ((Pair) rands).Car;
		  object opB = ((Pair) rands).D.Car;

		  Label loopTop = ilg.DefineLabel();
		  Label loopBreak = ilg.DefineLabel();

		  LocalBuilder theVec = ilg.DeclareLocal(typeof(System.Object[]));
		  cg.PushDotNetArg(ilg, opA, typeof(int), 0);
		  ilg.Emit(OpCodes.Newarr, typeof(System.Object));
		  ilg.Emit(OpCodes.Stloc, theVec);

		  LocalBuilder theIndex = ilg.DeclareLocal(typeof(int));
		  ilg.Emit(OpCodes.Ldc_I4_0);
		  ilg.Emit(OpCodes.Stloc, theIndex);

		  LocalBuilder theValue = ilg.DeclareLocal(typeof(object));
		  cg.PushDotNetArg(ilg, opB, null, 1);
		  ilg.Emit(OpCodes.Stloc, theValue);

		  ilg.MarkLabel(loopTop);
		  ilg.Emit(OpCodes.Ldloc, theIndex);
		  ilg.Emit(OpCodes.Ldloc, theVec);
		  ilg.Emit(OpCodes.Ldlen);
		  ilg.Emit(OpCodes.Bge, loopBreak);

		  ilg.Emit(OpCodes.Ldloc, theVec);
		  ilg.Emit(OpCodes.Ldloc, theIndex);
		  ilg.Emit(OpCodes.Ldloc, theValue);
		  ilg.Emit(OpCodes.Stelem_Ref);

		  ilg.Emit(OpCodes.Ldloc, theIndex);
		  ilg.Emit(OpCodes.Ldc_I4_1);
		  ilg.Emit(OpCodes.Add);
		  ilg.Emit(OpCodes.Stloc, theIndex);
		  ilg.Emit(OpCodes.Br, loopTop);

		  ilg.MarkLabel(loopBreak);
		  ilg.Emit(OpCodes.Ldloc, theVec);
		  break;
	      }

	      default:
		  throw new Exception("make-vector needs one or two arguments, not "+
				      argc);
	    }

	    return AssemblerState.VALUE;
	}
    }
}

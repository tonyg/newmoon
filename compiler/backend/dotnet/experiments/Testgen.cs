using System;
using System.IO;
using System.Collections;
using System.Reflection;
using System.Reflection.Emit;
using Newmoon;

public abstract class Testgen {
    public static void Main(String[] args) {
	String assemblyFilename = "testgenoutput.dll";
	String assemblyName = "testgenoutput";

	AssemblyName aName = new AssemblyName();
	aName.Name = assemblyName;
	String fqNamespace = "Newmoon.CompiledModules."+assemblyName;

	AssemblyBuilder assemblyBuilder =
	    AppDomain.CurrentDomain.DefineDynamicAssembly(aName,
							  AssemblyBuilderAccess.RunAndSave,
							  ".");

	ModuleBuilder moduleBuilder =
	    assemblyBuilder.DefineDynamicModule(fqNamespace+"."+assemblyName,
						assemblyFilename,
						true);

	TypeBuilder entryPoint =
	    moduleBuilder.DefineType(fqNamespace+"._"+assemblyName,
				     TypeAttributes.Public,
				     typeof(Newmoon.Module));

	ConstructorBuilder ccB =
	    entryPoint.DefineConstructor(MethodAttributes.Public |
					 MethodAttributes.Static |
					 MethodAttributes.HideBySig,
					 CallingConventions.Standard,
					 new Type[0] {});

	ILGenerator ilg = ccB.GetILGenerator();

	ilg.Emit(OpCodes.Ldc_I4_0);
	ilg.Emit(OpCodes.Ret);

	MethodBuilder mB =
	    moduleBuilder.DefineGlobalMethod("testGlobalMethod",
					     MethodAttributes.Public |
					     MethodAttributes.Static |
					     MethodAttributes.HideBySig,
					     CallingConventions.Standard,
					     typeof(object),
					     new Type[0] {});

	ilg = mB.GetILGenerator();
	ilg.Emit(OpCodes.Ldc_I4_1);
	ilg.Emit(OpCodes.Box, typeof(int));
	ilg.Emit(OpCodes.Ret);

	entryPoint.CreateType();
	moduleBuilder.CreateGlobalFunctions();

	assemblyBuilder.Save(assemblyFilename);
    }

    public abstract void Abs1(string i);
    public abstract void Abs2(object i);
}

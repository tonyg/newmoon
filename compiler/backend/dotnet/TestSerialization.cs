using System;
using System.IO;
using System.Collections;
using System.Resources;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using Newmoon;

public abstract class TestSerialization {
    public static void Main(String[] args) {
	String assemblyFilename = "testserialization.dll";
	String assemblyName = "testserialization";

	AssemblyName aName = new AssemblyName();
	aName.Name = assemblyName;
	String fqNamespace = "Newmoon.CompiledModules."+assemblyName;

	AssemblyBuilder assemblyBuilder =
	    AppDomain.CurrentDomain.DefineDynamicAssembly(aName,
							  AssemblyBuilderAccess.Save,
							  ".");

	assemblyBuilder.DefineDynamicModule(fqNamespace + "." + assemblyName,
					    assemblyFilename,
					    false);

	IResourceWriter rw = assemblyBuilder.DefineResource("literals",
							    "Literal Scheme Data",
							    "testserialization.resource");

	StringReader sr = new StringReader("(hello #(world 1 2) 3 4 \"five\" #t)");
	object o = Newmoon.Reader.Read(sr);
	o = new Pair(o, o);

	rw.AddResource("literal data", o);

	assemblyBuilder.Save(assemblyFilename);

	System.Console.WriteLine(o);

	FileStream fs = new FileStream("testserialization.output", FileMode.OpenOrCreate);
	BinaryFormatter bf = new BinaryFormatter();

	bf.Serialize(fs, o);
	fs.Close();

	fs = new FileStream("testserialization.output", FileMode.Open);
	bf = new BinaryFormatter();
	object p = bf.Deserialize(fs);

	System.Console.WriteLine(p);
	System.Console.WriteLine(p is Pair);
	System.Console.WriteLine(p == o);
	System.Console.WriteLine(((Pair) p).Car == ((Pair) p).Cdr);

	Assembly na = Assembly.LoadFrom(assemblyFilename);
	Stream rrs = na.GetManifestResourceStream("literals");
	ResourceSet rr = new ResourceSet(rrs);
	System.Console.WriteLine("p2 "+rr.GetObject("literal data"));
    }
}

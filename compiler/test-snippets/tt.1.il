// Input: tt.dll
// Image type: DLL
// Native code present: No
// 32-bit only: No
// Length of IL data: 2560

.assembly extern mscorlib
{
	.ver 1:0:5000:0
	.publickeytoken = (B7 7A 5C 56 19 34 E0 89)   // .z\V.4..
}
.assembly tt
{
	.hash algorithm 0x00008004
	.ver 0:0:0:0
}
.module tt.dll
// MVID: {A1CF4111-C0D6-4A5C-A422-5D7661FED2A0}
.namespace MyNamespace
{
.class public auto ansi beforefieldinit Zot extends [mscorlib]System.Object
{
	.method public hidebysig specialname rtspecialname instance void .ctor() cil managed 
	{
		// Start of method header: 20ec
		.maxstack  8
	?L20ed:
		ldarg.0
		call       instance void [mscorlib]System.Object::.ctor()
		ret
	}
	.method public static hidebysig void RecCS(int32 n) cil managed 
	{
		// Start of method header: 20f4
		.maxstack  8
	?L20f5:
		ldarg.0
		brtrue     ?L2105
		call       void MyNamespace.Zot::Die()
		br         ?L210d
	?L2105:
		ldarg.0
		ldc.i4.1
		sub
		call       void MyNamespace.Zot::RecCS(int32)
	?L210d:
		ret
	}
	.method public static hidebysig void Die() cil managed 
	{
		// Start of method header: 2110
		.maxstack  8
	?L2111:
		ldstr      "Argh"
		newobj     instance void [mscorlib]System.ApplicationException::.ctor(class System.String)
		throw
	}
}
}

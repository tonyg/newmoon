.assembly extern mscorlib
{
  .ver 1:0:5000:0
}
.assembly 'tt'
{
  .hash algorithm 0x00008004
  .ver  0:0:0:0
}
.module tt.dll // GUID = {A1CF4111-C0D6-4A5C-A422-5D7661FED2A0}


.namespace MyNamespace
{
  .class public auto ansi beforefieldinit Zot
  	extends [mscorlib]System.Object
  {

    // method line 1
    .method public hidebysig  specialname  rtspecialname 
           instance default void .ctor ()  cil managed 
    {
        // Method begins at RVA 0x20ec
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void valuetype [mscorlib]System.Object::.ctor()
	IL_0006:  ret 
    } // end of method Zot::instance default void .ctor () 

    // method line 2
    .method public static  hidebysig 
           default void RecCS (int32 n)  cil managed 
    {
	.param [1]
        // Method begins at RVA 0x20f4
	// Code size 25 (0x19)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  brtrue IL_0010

	IL_0006:  call void class MyNamespace.Zot::Die()
	IL_000b:  br IL_0018

	IL_0010:  ldarg.0 
	IL_0011:  ldc.i4.1 
	IL_0012:  sub 
	IL_0013:  call void class MyNamespace.Zot::RecCS(int32)
	IL_0018:  ret 
    } // end of method Zot::default void RecCS (int32 n) 

    // method line 3
    .method public static  hidebysig 
           default void Die ()  cil managed 
    {
        // Method begins at RVA 0x2110
	// Code size 11 (0xb)
	.maxstack 8
	IL_0000:  ldstr "Argh"
	IL_0005:  newobj instance void class [mscorlib]System.ApplicationException::.ctor(string)
	IL_000a:  throw 
    } // end of method Zot::default void Die () 

  } // end of class MyNamespace.Zot
}


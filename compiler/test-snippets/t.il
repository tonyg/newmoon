.class public auto ansi serializable beforefieldinit Foo
{
	.field public object bar

	.method private hidebysig specialname rtspecialname static default void Rec1(int32 n) cil managed
	{
		.param [1]

		ldarg.0
		brtrue nonzero
		tail.call void class [tt]Zot::Die()
		ret

	nonzero:
		ldarg.0
		ldc.i4.1
		sub
		call void class Foo::Rec2(int32)
		ret
	}

	.method private hidebysig specialname rtspecialname static default void Rec2(int32 n) cil managed
	{
		.param [1]

		ldstr "hi"
		call void class [mscorlib]System.Console::WriteLine(string)
		ldarg.0
		ldc.i4.1
		sub
		tail.call void class Foo::Rec1(int32)
		ret
	}

	.method public hidebysig specialname rtspecialname static default void Main(string[] args) cil managed
	{
		.param [1]
		.entrypoint

		ldc.i4	10
		call void class Foo::Rec1(int32)
		ret
	}
}

.assembly 'ttt' {}
.assembly extern mscorlib {}

.namespace TTT
{

.class public auto ansi beforefieldinit TTT
	extends [mscorlib]System.Object
{
	.method public hidebysig instance default
	void NonVirt(object x)
	cil managed
	{
		ldarg	x
		ret
	}

	.method public virtual newslot hidebysig instance default
	void Virt(object x, object y)
	cil managed
	{
		ldarg	x
		ldarg	y
		ret
	}

	.method public hidebysig specialname rtspecialname instance default
	void .ctor(object x)
	cil managed
	{
		ldarg	x
		ret
	}

	.method public static hidebysig default
	void Stat(object x)
	{
		ldarg	x
		ret
	}
}

}

.namespace Newmoon
{

.class public auto ansi abstract beforefieldinit Closure
       extends object
{
	.method public hidebysig specialname rtspecialname instance default
	void .ctor() cil managed
	{
		ldarg.0
		tail.call	instance void object::.ctor()
		ret
	}

	.method public hidebysig specialname rtspecialname instance
	object Apply(class Newmoon.Closure k) cil managed
	{
		ldarg.0
		ldc.i4		0
		newarr		object
		tail.callvirt	instance object class Newmoon.Closure::ApplyVarargs(class Newmoon.Closure, object[])
		ret
	}

	.method public hidebysig specialname rtspecialname instance
	object Apply(class Newmoon.Closure k, object a) cil managed
	{
		ldarg.0
		ldc.i4		1
		newarr		object
		dup
		ldarg.1
		stelem.ref
		tail.callvirt	instance object class Newmoon.Closure::ApplyVarargs(class Newmoon.Closure, object[])
		ret
	}

	.method public hidebysig specialname rtspecialname instance
	object Apply(class Newmoon.Closure k, object a, object b) cil managed
	{
		ldarg.0
		ldc.i4		2
		newarr		object
		dup
		ldarg.1
		stelem.ref
		dup
		ldarg.2
		stelem.ref
		tail.callvirt	instance object class Newmoon.Closure::ApplyVarargs(class Newmoon.Closure, object[])
		ret
	}

	.method public hidebysig specialname rtspecialname instance
	object Apply(class Newmoon.Closure k, object a, object b, object c) cil managed
	{
		ldarg.0
		ldc.i4		3
		newarr		object
		dup
		ldarg.1
		stelem.ref
		dup
		ldarg.2
		stelem.ref
		dup
		ldarg.3
		stelem.ref
		tail.callvirt	instance object class Newmoon.Closure::ApplyVarargs(class Newmoon.Closure, object[])
		ret
	}

	.method public hidebysig specialname rtspecialname instance
	object Apply(class Newmoon.Closure k, object a, object b, object c, object d) cil managed
	{
		ldarg.0
		ldc.i4		4
		newarr		object
		dup
		ldarg.1
		stelem.ref
		dup
		ldarg.2
		stelem.ref
		dup
		ldarg.3
		stelem.ref
		dup
		ldarg		4
		stelem.ref
		tail.callvirt	instance object class Newmoon.Closure::ApplyVarargs(class Newmoon.Closure, object[])
		ret
	}

	.method public virtual hidebysig newslot abstract specialname instance default
	object ApplyVarargs(class Newmoon.Closure k, object[] args) cil managed
	{
	}
}

.class public auto ansi beforefieldinit ToplevelContinuation
       extends Newmoon.Closure
{
	.method public hidebysig specialname rtspecialname instance default
	void .ctor() cil managed
	{
		ldarg.0
		tail.call	instance void object::.ctor()
		ret
	}

	.method public virtual hidebysig instance default
	object ApplyVarargs(class Newmoon.Closure k, object[] args) cil managed
	{
		ldarg.1
		ldc.i4		0
		ldelem.ref
		ret
	}
}

}
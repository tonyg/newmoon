.assembly 'NewmoonLowlevel' {}
.assembly extern 'NewmoonHighlevel' {}
.assembly extern mscorlib {}

.namespace Newmoon
{

.class public auto ansi abstract beforefieldinit Closure
	extends [mscorlib]System.Object
{
	.method public hidebysig specialname rtspecialname instance default
	void .ctor() cil managed
	{
		ldarg.0
		call	instance void object::.ctor()
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Apply(class [NewmoonHighlevel]Newmoon.Module module, class Newmoon.Continuation k)
	cil managed
	{
		ldarg.0
		ldarg		1	// module
		ldarg		2	// k
		ldc.i4		0
		newarr		object
		tail.
		callvirt	instance object class Newmoon.Closure::ApplyVarargs(class [NewmoonHighlevel]Newmoon.Module, class Newmoon.Continuation, object[])
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Apply(class [NewmoonHighlevel]Newmoon.Module module, class Newmoon.Continuation k,
		object a)
	cil managed
	{
		ldarg.0
		ldarg		1	// module
		ldarg		2	// k
		ldc.i4		1
		newarr		object
		dup
		ldc.i4		0
		ldarg		3	// a
		stelem.ref
		tail.
		callvirt	instance object class Newmoon.Closure::ApplyVarargs(class [NewmoonHighlevel]Newmoon.Module, class Newmoon.Continuation, object[])
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Apply(class [NewmoonHighlevel]Newmoon.Module module, class Newmoon.Continuation k,
		object a, object b)
	cil managed
	{
		ldarg.0
		ldarg		1	// module
		ldarg		2	// k
		ldc.i4		2
		newarr		object
		dup
		ldc.i4		0
		ldarg		3	// a
		stelem.ref
		dup
		ldc.i4		1
		ldarg		4	// b
		stelem.ref
		tail.
		callvirt	instance object class Newmoon.Closure::ApplyVarargs(class [NewmoonHighlevel]Newmoon.Module, class Newmoon.Continuation, object[])
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Apply(class [NewmoonHighlevel]Newmoon.Module module, class Newmoon.Continuation k,
		object a, object b, object c)
	cil managed
	{
		ldarg.0
		ldarg		1	// module
		ldarg		2	// k
		ldc.i4		3
		newarr		object
		dup
		ldc.i4		0
		ldarg		3	// a
		stelem.ref
		dup
		ldc.i4		1
		ldarg		4	// b
		stelem.ref
		dup
		ldc.i4		2
		ldarg		5	// c
		stelem.ref
		tail.
		callvirt	instance object class Newmoon.Closure::ApplyVarargs(class [NewmoonHighlevel]Newmoon.Module, class Newmoon.Continuation, object[])
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Apply(class [NewmoonHighlevel]Newmoon.Module module, class Newmoon.Continuation k,
		object a, object b, object c, object d)
	cil managed
	{
		ldarg.0
		ldarg		1	// module
		ldarg		2	// k
		ldc.i4		4
		newarr		object
		dup
		ldc.i4		0
		ldarg		3	// a
		stelem.ref
		dup
		ldc.i4		1
		ldarg		4	// b
		stelem.ref
		dup
		ldc.i4		2
		ldarg		5	// c
		stelem.ref
		dup
		ldc.i4		3
		ldarg		6	// d
		stelem.ref
		tail.
		callvirt	instance object class Newmoon.Closure::ApplyVarargs(class [NewmoonHighlevel]Newmoon.Module, class Newmoon.Continuation, object[])
		ret
	}

	.method public virtual hidebysig newslot abstract instance default
	object ApplyVarargs(class [NewmoonHighlevel]Newmoon.Module module, class Newmoon.Continuation k, object[] args)
	cil managed
	{
	}
}

.class public auto ansi abstract beforefieldinit Continuation
       extends [mscorlib]System.Object
{
	.method public hidebysig specialname rtspecialname instance default
	void .ctor() cil managed
	{
		ldarg.0
		call	instance void object::.ctor()
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Reply(class [NewmoonHighlevel]Newmoon.Module module)
	cil managed
	{
		ldarg.0
		ldarg		1	// module
		ldc.i4		0
		newarr		object
		tail.
		callvirt	instance object class Newmoon.Continuation::ReplyVarargs(class [NewmoonHighlevel]Newmoon.Module, object[])
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Reply(class [NewmoonHighlevel]Newmoon.Module module,
		object a)
	cil managed
	{
		ldarg.0
		ldarg		1	// module
		ldc.i4		1
		newarr		object
		dup
		ldc.i4		0
		ldarg		2	// a
		stelem.ref
		tail.
		callvirt	instance object class Newmoon.Continuation::ReplyVarargs(class [NewmoonHighlevel]Newmoon.Module, object[])
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Reply(class [NewmoonHighlevel]Newmoon.Module module,
		object a, object b)
	cil managed
	{
		ldarg.0
		ldarg		1	// module
		ldc.i4		2
		newarr		object
		dup
		ldc.i4		0
		ldarg		2	// a
		stelem.ref
		dup
		ldc.i4		1
		ldarg		3	// b
		stelem.ref
		tail.
		callvirt	instance object class Newmoon.Continuation::ReplyVarargs(class [NewmoonHighlevel]Newmoon.Module, object[])
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Reply(class [NewmoonHighlevel]Newmoon.Module module,
		object a, object b, object c)
	cil managed
	{
		ldarg.0
		ldarg		1	// module
		ldc.i4		3
		newarr		object
		dup
		ldc.i4		0
		ldarg		2	// a
		stelem.ref
		dup
		ldc.i4		1
		ldarg		3	// b
		stelem.ref
		dup
		ldc.i4		2
		ldarg		4	// c
		stelem.ref
		tail.
		callvirt	instance object class Newmoon.Continuation::ReplyVarargs(class [NewmoonHighlevel]Newmoon.Module, object[])
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Reply(class [NewmoonHighlevel]Newmoon.Module module,
		object a, object b, object c, object d)
	cil managed
	{
		ldarg.0
		ldarg		1	// module
		ldc.i4		4
		newarr		object
		dup
		ldc.i4		0
		ldarg		2	// a
		stelem.ref
		dup
		ldc.i4		1
		ldarg		3	// b
		stelem.ref
		dup
		ldc.i4		2
		ldarg		4	// c
		stelem.ref
		dup
		ldc.i4		3
		ldarg		5	// d
		stelem.ref
		tail.
		callvirt	instance object class Newmoon.Continuation::ReplyVarargs(class [NewmoonHighlevel]Newmoon.Module, object[])
		ret
	}

	.method public virtual hidebysig newslot abstract instance default
	object ReplyVarargs(class [NewmoonHighlevel]Newmoon.Module module, object[] args)
	cil managed
	{
	}
}

.class public auto ansi beforefieldinit ToplevelContinuation
       extends Newmoon.Continuation
{
	.method public hidebysig specialname rtspecialname instance default
	void .ctor()
	cil managed
	{
		ldarg.0
		call	instance void object::.ctor()
		ret
	}

	.method public virtual hidebysig newslot instance default
	object ReplyVarargs(class [NewmoonHighlevel]Newmoon.Module module, object[] args)
	cil managed
	{
		ldarg.1
		ldc.i4		0
		ldelem.ref
		ret
	}
}

}

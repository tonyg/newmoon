.namespace Newmoon
{

.class public auto ansi abstract beforefieldinit TailClosure
	extends [Newmoon]Newmoon.Closure
{
	.method public hidebysig specialname rtspecialname instance default
	void .ctor(class [Newmoon]Newmoon.Module module) cil managed
	{
		ldarg.0
		ldarg		1	// module
		call		instance void class [Newmoon]Newmoon.Closure::.ctor(class [Newmoon]Newmoon.Module)
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Apply(class [Newmoon]Newmoon.Continuation k)
	cil managed
	{
		ldarg.0
		ldarg		1	// k
		ldc.i4		0
		newarr		object
		tail.
		callvirt	instance object class [Newmoon]Newmoon.Closure::ApplyVarargs(class [Newmoon]Newmoon.Continuation, object[])
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Apply(class [Newmoon]Newmoon.Continuation k,
		object a)
	cil managed
	{
		ldarg.0
		ldarg		1	// k
		ldc.i4		1
		newarr		object
		dup
		ldc.i4		0
		ldarg		2	// a
		stelem.ref
		tail.
		callvirt	instance object class [Newmoon]Newmoon.Closure::ApplyVarargs(class [Newmoon]Newmoon.Continuation, object[])
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Apply(class [Newmoon]Newmoon.Continuation k,
		object a, object b)
	cil managed
	{
		ldarg.0
		ldarg		1	// k
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
		callvirt	instance object class [Newmoon]Newmoon.Closure::ApplyVarargs(class [Newmoon]Newmoon.Continuation, object[])
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Apply(class [Newmoon]Newmoon.Continuation k,
		object a, object b, object c)
	cil managed
	{
		ldarg.0
		ldarg		1	// k
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
		callvirt	instance object class [Newmoon]Newmoon.Closure::ApplyVarargs(class [Newmoon]Newmoon.Continuation, object[])
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Apply(class [Newmoon]Newmoon.Continuation k,
		object a, object b, object c, object d)
	cil managed
	{
		ldarg.0
		ldarg		1	// k
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
		callvirt	instance object class [Newmoon]Newmoon.Closure::ApplyVarargs(class [Newmoon]Newmoon.Continuation, object[])
		ret
	}

	.method public virtual hidebysig newslot abstract instance default
	object ApplyVarargs(class [Newmoon]Newmoon.Continuation k, object[] args)
	cil managed
	{
	}
}

.class public auto ansi abstract beforefieldinit TailContinuation
       extends [Newmoon]Newmoon.Continuation
{
	.method public hidebysig specialname rtspecialname instance default
	void .ctor(class [Newmoon]Newmoon.Module module) cil managed
	{
		ldarg.0
		ldarg		1	// module
		call		instance void class [Newmoon]Newmoon.Continuation::.ctor(class [Newmoon]Newmoon.Module)
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Reply()
	cil managed
	{
		ldarg.0
		ldc.i4		0
		newarr		object
		tail.
		callvirt	instance object class [Newmoon]Newmoon.Continuation::ReplyVarargs(object[])
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Reply(object a)
	cil managed
	{
		ldarg.0
		ldc.i4		1
		newarr		object
		dup
		ldc.i4		0
		ldarg		1	// a
		stelem.ref
		tail.
		callvirt	instance object class [Newmoon]Newmoon.Continuation::ReplyVarargs(object[])
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Reply(object a, object b)
	cil managed
	{
		ldarg.0
		ldc.i4		2
		newarr		object
		dup
		ldc.i4		0
		ldarg		1	// a
		stelem.ref
		dup
		ldc.i4		1
		ldarg		2	// b
		stelem.ref
		tail.
		callvirt	instance object class [Newmoon]Newmoon.Continuation::ReplyVarargs(object[])
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Reply(object a, object b, object c)
	cil managed
	{
		ldarg.0
		ldc.i4		3
		newarr		object
		dup
		ldc.i4		0
		ldarg		1	// a
		stelem.ref
		dup
		ldc.i4		1
		ldarg		2	// b
		stelem.ref
		dup
		ldc.i4		2
		ldarg		3	// c
		stelem.ref
		tail.
		callvirt	instance object class [Newmoon]Newmoon.Continuation::ReplyVarargs(object[])
		ret
	}

	.method public virtual hidebysig newslot instance default
	object Reply(object a, object b, object c, object d)
	cil managed
	{
		ldarg.0
		ldc.i4		4
		newarr		object
		dup
		ldc.i4		0
		ldarg		1	// a
		stelem.ref
		dup
		ldc.i4		1
		ldarg		2	// b
		stelem.ref
		dup
		ldc.i4		2
		ldarg		3	// c
		stelem.ref
		dup
		ldc.i4		3
		ldarg		4	// d
		stelem.ref
		tail.
		callvirt	instance object class [Newmoon]Newmoon.Continuation::ReplyVarargs(object[])
		ret
	}

	.method public virtual hidebysig newslot abstract instance default
	object ReplyVarargs(object[] args)
	cil managed
	{
	}
}

}

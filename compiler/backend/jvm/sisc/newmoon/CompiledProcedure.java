package sisc.newmoon;

public abstract class CompiledProcedure extends sisc.data.Procedure {
    /*
    private static void dvlr(Object[] vlr) {
	if (vlr == null) {
	    System.out.println("(no vlr)");
	} else {
	    System.out.print(vlr.length);
	    for (int i = 0; i < vlr.length; i++) {
		System.out.print("; " + vlr[i]);
	    }
	    System.out.println();
	}
    }
    */

    public void apply(sisc.interpreter.Interpreter r)
	throws sisc.interpreter.ContinuationException
    {
	sisc.data.Value[] argv;

	if (r.stk == null) {
	    //System.out.println("NULL stk so no argv-modification");
	    argv = r.vlr;
	} else {
	    argv = r.createValues(r.vlr.length + 1);
	    System.arraycopy(r.vlr, 0, argv, 1, r.vlr.length);
	    //System.out.println("0 " + this);
	    //System.out.println("1 " + argv);
	    //System.out.println("2 " + r);
	    //System.out.println("3 " + r.stk);
	    argv[0] = r.stk.capture(r);
	    r.stk = null; // %%% is this sufficient? (mostly worried about
			  // safe-for-space)
	}

	r.compiledContinuation = this;
	r.vlr = argv;
	r.nxp = null;
	r.acc = null;

	while (r.compiledContinuation != null) {
	    //System.out.print("In loop " + r.compiledContinuation + " and ");
	    //dvlr(r.vlr);
	    r.compiledContinuation.applyCompiled(r.vlr, r);
	}
	//System.out.print("Out of loop, resuming with " + r.nxp + ", " + r.acc + " and ");
	//dvlr(r.vlr);
    }

    // Reintroduce applyCompiled as abstract at this level.
    public abstract void applyCompiled(sisc.data.Value[] argv, sisc.interpreter.Interpreter r);

    public void display(sisc.io.ValueWriter w)
	throws java.io.IOException
    {
	w.append(this.getClass().getName());
    }

    public static void checkVarArgc(sisc.data.Value[] argv, int requiredLength) {
	if (argv.length < requiredLength) {
	    throwArgSizeException();
	}
    }

    public static void checkArgc(sisc.data.Value[] argv, int requiredLength) {
	if (argv.length != requiredLength) {
	    throwArgSizeException();
	}
    }
}

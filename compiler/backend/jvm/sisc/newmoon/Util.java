package sisc.newmoon;

public class Util {
    private static sisc.reader.Parser parser = new sisc.reader.Parser(new sisc.reader.Lexer());

    public static sisc.data.Quantity numberLiteral(String str) {
	synchronized (parser) {
	    try {
		return (sisc.data.Quantity)
		    parser.nextExpression
		    (new sisc.io.ReaderInputPort
		     (new java.io.StringReader(str)));
	    } catch (java.io.IOException ioe) {
		// What on earth are we going to do with this, here, now?
		return null;
	    }
	}
    }

    public static sisc.data.Value lookupGlobal(sisc.interpreter.Interpreter r,
					       sisc.data.Value name)
    {
	sisc.data.Value v = r.getCtx().toplevel_env.lookup((sisc.data.Symbol) name);
	if (v == null)
	    throw new RuntimeException("newmoon.Util.lookupGlobal: No global with name: "+name);
	return v;
    }

    public static sisc.data.Value setGlobal(sisc.interpreter.Interpreter r,
					    sisc.data.Value name,
					    sisc.data.Value value)
    {
	r.getCtx().toplevel_env.define((sisc.data.Symbol) name, value); // %%% should be set!
	return sisc.data.SchemeVoid.VOID;
    }

    public static sisc.data.Value defineGlobal(sisc.interpreter.Interpreter r,
					       sisc.data.Value name,
					       sisc.data.Value kind,
					       sisc.data.Value value)
    {
	System.out.println("Defining "+name.toString()+" as kind "+kind.toString()+" and value "+value.toString());
	if ("global".equals(kind.toString())) {
	    r.getCtx().toplevel_env.define((sisc.data.Symbol) name, value);
	}
	return sisc.data.SchemeVoid.VOID;
    }
}

package sisc.newmoon;

import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.*;

public class ClassLoader extends java.lang.ClassLoader {
    private Class[] classes;

    public ClassLoader(ClassGen[] cgs) {
	classes = new Class[cgs.length];

	for (int i = 0; i < cgs.length; i++) {
	    ClassGen cg = cgs[i];
	    JavaClass jc = cg.getJavaClass();
	    byte[] classbytes = jc.getBytes();
	    //System.out.println("Defining class " + i);
	    classes[i] = defineClass(null, classbytes, 0, classbytes.length);
	}

	for (int i = 0; i < classes.length; i++) {
	    //System.out.println("Resolving class " + i + " " + classes[i]);
	    resolveClass(classes[i]);
	}
    }

    public sisc.data.Procedure getRootLambda()
	throws InstantiationException,
	       IllegalAccessException
    {
	return (sisc.data.Procedure) classes[0].newInstance();
    }
}

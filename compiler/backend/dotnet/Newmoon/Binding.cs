using System;
using System.Collections;

namespace Newmoon {
    public class Binding {
	public readonly string name;
	public readonly string kind;
	public Object value;

	public Binding(string n, string k) {
	    name = n;
	    kind = k;
	    value = Undefined.UNDEFINED;
	}

	public Object getValue() {
	    return value;
	}

	public override string ToString() {
	    return "#!binding<"+name+":="+value.ToString()+">";
	}
    }
}

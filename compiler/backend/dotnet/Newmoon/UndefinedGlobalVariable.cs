using System;
using System.Collections;

namespace Newmoon {
    public class UndefinedGlobalVariable: Exception {
	public UndefinedGlobalVariable(Binding var)
	    : base("Undefined global "+var.name)
	{}
    }
}

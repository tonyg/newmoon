using System;
using System.Collections;
using System.Reflection.Emit;

namespace Newmoon {
    public interface ICodeGenerator {
	object NodeGet(object node, string kind, string key);
	AssemblerState GenNode(ILGenerator ilg, object node);
	void GenLoadLiteral(ILGenerator ilg, object literal);
	FieldBuilder RecordBinding(string name);
	void PushDotNetArg(ILGenerator ilg, object node, Type t, int pos);
	int PushDotNetArgs(ILGenerator ilg, List rands, Type[] types, int firstPos);
    }
}

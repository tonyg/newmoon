using System;
using System.Collections;
using System.Reflection.Emit;

namespace Newmoon {
    public enum AssemblerState {
	RETURNED,
	VALUE,
	NOVALUE
    };

    public interface IAssembler {
	AssemblerState Assemble(ILGenerator ilg, ICodeGenerator cg,
				Pair node, List flags, string name, List args);
    }
}

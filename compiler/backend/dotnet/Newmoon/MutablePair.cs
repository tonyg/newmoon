using System;
using System.Collections;

namespace Newmoon {
    public class MutablePair: Pair {
	public MutablePair() : this(Null.NULL, Null.NULL) {}
	public MutablePair(Object a, Object d) : base(a, d) {}
    }
}

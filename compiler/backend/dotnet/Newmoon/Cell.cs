using System;
using System.Collections;

namespace Newmoon {
    public class Cell {
	public Object value;

	public Cell(Object v) {
	    value = v;
	}

	public override string ToString() {
	    return "#!cell<"+value.ToString()+">";
	}
    }
}

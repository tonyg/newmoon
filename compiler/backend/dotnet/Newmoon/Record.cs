using System;

namespace Newmoon {
    [Serializable]
    public class Record {
	private object[] slots;

	public Record(int size) {
	    slots = new object[size];
	}

	public object this[int index] {
	    get {
		return slots[index];
	    }
	    set {
		slots[index] = value;
	    }
	}
    }
}

using System;
using System.Collections;
using System.Runtime.Serialization;

namespace Newmoon {
    [Serializable]
    public class Null: List, IEnumerable, ISerializable {
	private Null() {}
	public static readonly Null NULL = new Null();

	public override IEnumerator GetEnumerator() {
	    return (IEnumerator) new NullEnumerator();
	}

	public override String ToString() {
	    return "()";
	}

	void ISerializable.GetObjectData(SerializationInfo info,
					 StreamingContext context)
	{
	    info.SetType(typeof(NullSerializationHelper));
	}
    }

    [Serializable]
    internal sealed class NullSerializationHelper: IObjectReference {
	public Object GetRealObject(StreamingContext context) {
	    return Null.NULL;
	}
    }

    public class NullEnumerator: IEnumerator {
	public virtual object Current {
	    get { return Null.NULL; }
	}

	public virtual bool MoveNext() {
	    return false;
	}

	public virtual void Reset() {
	}
    }
}

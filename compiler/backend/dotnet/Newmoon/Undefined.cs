using System;
using System.Collections;
using System.Runtime.Serialization;

namespace Newmoon {
    [Serializable]
    public class Undefined: ISerializable {
	private Undefined() {}
	public static readonly Undefined UNDEFINED = new Undefined();

	public override String ToString() {
	    return "#<undefined>";
	}

	public static object GetUndefined() {
	    return UNDEFINED;
	}

	void ISerializable.GetObjectData(SerializationInfo info,
					 StreamingContext context)
	{
	    info.SetType(typeof(UndefinedSerializationHelper));
	}
    }

    [Serializable]
    internal sealed class UndefinedSerializationHelper: IObjectReference {
	public Object GetRealObject(StreamingContext context) {
	    return Undefined.UNDEFINED;
	}
    }
}

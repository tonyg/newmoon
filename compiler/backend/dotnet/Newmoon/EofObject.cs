using System;
using System.Collections;
using System.Runtime.Serialization;

namespace Newmoon {
    [Serializable]
    public sealed class EofObject: ISerializable {
	private EofObject() {}

	private static EofObject eof = new EofObject();

	public static EofObject EOF {
	    get { return eof; }
	}

	public static EofObject GetEof() {
	    return eof;
	}

	void ISerializable.GetObjectData(SerializationInfo info,
					 StreamingContext context)
	{
	    info.SetType(typeof(EofObjectSerializationHelper));
	}
    }

    [Serializable]
    internal sealed class EofObjectSerializationHelper: IObjectReference {
	public Object GetRealObject(StreamingContext context) {
	    return EofObject.EOF;
	}
    }
}

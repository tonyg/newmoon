using System;
using System.Collections;
using System.Text;

namespace Newmoon {
    [Serializable]
    public class SchemeString: ICloneable {
	private string _string;
	private char[] _chars;
	private bool _mutable;

	public SchemeString() {
	    _string = "";
	    _chars = null;
	    _mutable = true;
	}

	public SchemeString(int repeat, char c) {
	    _string = null;
	    _chars = new char[repeat];
	    for (int i = 0; i < repeat; i++)
		_chars[i] = c;
	    _mutable = true;
	}

	public SchemeString(int repeat)
	    : this(repeat, ' ')
	{}

	public SchemeString(string s) : this(s, true) {}

	public SchemeString(string s, bool m) {
	    _string = s;
	    _chars = null;
	    _mutable = m;
	}

	public SchemeString(StringBuilder s, bool m) {
	    _string = s.ToString();
	    _chars = null;
	    _mutable = m;
	}

	public SchemeString(char[] c, bool m) {
	    _string = null;
	    _chars = c;
	    _mutable = m;
	}

	public SchemeString(SchemeString s, bool m) {
	    Assign(s, m);
	}

	private bool isNative() {
	    return _string != null;
	}

	private bool isArray() {
	    return _chars != null;
	}

	private void ensureArray() {
	    if (!isArray()) {
		_chars = _string.ToCharArray();
		_string = null;
	    }
	}

	private void ensureNative() {
	    if (!isNative()) {
		_string = new String(_chars);
		_chars = null;
	    }
	}

	public int Length {
	    get { return isNative() ? _string.Length : _chars.Length; }
	}

	public char this[int index] {
	    get { return isNative() ? _string[index] : _chars[index]; }
	    set {
		ensureArray();
		if (!_mutable)
		    throw new InvalidOperationException("Attempt to string-set! "+
							"immutable string: "+this+" "+index);
		_chars[index] = value;
	    }
	}

	public bool Mutable {
	    get { return _mutable; }
	    set { _mutable = value; }
	}

	public void Assign(SchemeString s, bool m) {
	    if (s.isNative()) {
		_string = s._string;
		_chars = null;
	    } else {
		_string = null;
		_chars = new char[s._chars.Length];
		System.Array.Copy(s._chars, _chars, _chars.Length);
	    }
	    _mutable = m;
	}

	public override string ToString() {
	    ensureNative();
	    return _string;
	}

	public char[] GetCharArray() {
	    ensureArray();
	    return _chars;
	}

	public void Fill(char c) {
	    if (!isArray()) {
		_chars = new char[_string.Length];
		_string = null;
	    }
	    for (int i = 0; i < _chars.Length; i++) {
		_chars[i] = c;
	    }
	}

	public SchemeString SubString(int start, int end) {
	    if (start < 0)
		throw new ArgumentOutOfRangeException("start", start, "substring start < 0");
	    if (end > Length)
		throw new ArgumentOutOfRangeException("end", end, "substring end > string-length");
	    if (start > end)
		throw new ArgumentOutOfRangeException("start", start, "substring start > end");

	    ensureArray();
	    char[] s = new char[end - start];
	    for (int i = start; i < end; i++)
		s[i - start] = _chars[i];
	    return new SchemeString(s, true);
	}

	public static SchemeString FromList(List l) {
	    StringBuilder b = new StringBuilder();
	    foreach (Char c in l) {
		b.Append(c);
	    }
	    return new SchemeString(b, true);
	}

	public List ToList() {
	    ensureArray();
	    List result = Null.NULL;
	    for (int i = _chars.Length - 1; i >= 0; i--)
		result = new MutablePair(_chars[i], result);
	    return result;
	}

	public static SchemeString Join(List l, SchemeString sep) {
	    StringBuilder b = new StringBuilder();
	    bool haveFirst = false;

	    sep.ensureNative();

	    foreach (SchemeString s in l) {
		if (haveFirst)
		    b.Append(sep._string);

		if (s.isNative()) {
		    b.Append(s._string);
		} else {
		    b.Append(s._chars);
		}

		haveFirst = true;
	    }

	    return new SchemeString(b, true);
	}

	public override bool Equals(object b) {
	    if (!(b is SchemeString))
		return false;
	    ensureNative();
	    SchemeString bb = (SchemeString) b;
	    bb.ensureNative();
	    return _string.Equals(bb._string);
	}

	public override int GetHashCode() {
	    ensureNative();
	    return _string.GetHashCode();
	}

	public object Clone() {
	    return new SchemeString(this, _mutable);
	}
    }
}

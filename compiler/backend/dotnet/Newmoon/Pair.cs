using System;
using System.Collections;
using System.Text;

namespace Newmoon {
    [Serializable]
    public class Pair: List, IEnumerable {
	protected Object car;
	protected Object cdr;

	public Pair() : this(Null.NULL, Null.NULL) {}

	public Pair(Object a, Object d) {
	    car = a;
	    cdr = d;
	}

	public static Pair xcons(Object d, Object a) {
	    return new Pair(a, d);
	}

	public override IEnumerator GetEnumerator() {
	    return (IEnumerator) new PairEnumerator(this);
	}

	public MutablePair Mutable {
	    get {
		if (this is MutablePair) {
		    return (MutablePair) this;
		} else {
		    throw new InvalidOperationException("Attempt to obtain Newmoon.MutablePair from immutable pair");
		}
	    }
	}

	public Object Car {
	    get { return car; }
	    set { Mutable.car = value; }
	}

	public Object Cdr {
	    get { return cdr; }
	    set { Mutable.cdr = value; }
	}

	public Pair A {
	    get { return (Pair) car; }
	}

	public Pair D {
	    get { return (Pair) cdr; }
	}

	public override bool Equals(object o) {
	    if (!(o is Pair))
		return false;
	    Pair p = (Pair) o;
	    return p.car.Equals(car) && p.cdr.Equals(cdr);
	}

	public override int GetHashCode() {
	    return car.GetHashCode() + cdr.GetHashCode();
	}

	private string StringFor(object o) {
	    if (o == null)
		return "#!void";
	    else
		return o.ToString();
	}

	public override string ToString() {
	    StringBuilder s = new StringBuilder();

	    s.Append('(');
	    s.Append(StringFor(this.Car));

	    object p = this.Cdr;

	    while (p is Pair) {
		s.Append(' ');
		s.Append(StringFor(((Pair) p).Car));
		p = ((Pair) p).Cdr;
	    }

	    if (p != Null.NULL) {
		s.Append(" . ");
		s.Append(StringFor(p));
	    }

	    s.Append(')');
	    return s.ToString();
	}
    }

    public class PairEnumerator: IEnumerator {
	private Pair start;
	private Pair current;

	public PairEnumerator(Pair start) {
	    this.start = start;
	    this.current = null;
	}

	public virtual object Current {
	    get { return current.Car; }
	}

	public virtual bool MoveNext() {
	    if (current == null) {
		current = start;
		return true;
	    } else {
		object next = current.Cdr;
		if (next != Null.NULL) {
		    if (next is Pair) {
			current = (Pair) next;
			return true;
		    } else {
			throw new InvalidOperationException("Attempt to enumerate a Pair that is not a list");
		    }
		} else {
		    return false;
		}
	    }
	}

	public virtual void Reset() {
	    current = null;
	}
    }
}

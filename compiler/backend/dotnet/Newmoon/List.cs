using System;
using System.Collections;

namespace Newmoon {
    [Serializable]
    public abstract class List: IEnumerable {
	public abstract IEnumerator GetEnumerator();

	public int ListLength() {
	    int count = 0;
	    foreach (object e in this) {
		count++;
	    }
	    return count;
	}

	public object[] ToVector() {
	    object[] result = new object[ListLength()];
	    int count = 0;
	    foreach (object e in this) {
		result[count++] = e;
	    }
	    return result;
	}

	public static List FromVector(object[] vec) {
	    List x = Null.NULL;
	    for (int i = vec.Length - 1; i >= 0; i--) {
		x = new Pair(vec[i], x);
	    }
	    return x;
	}

	public static object AssQ(object key, List l) {
	    foreach (Pair cell in l) {
		if (cell.Car == key)
		    return cell;
	    }
	    return false;
	}
    }
}

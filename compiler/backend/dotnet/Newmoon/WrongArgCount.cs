using System;
using System.Collections;

namespace Newmoon {
    public class WrongArgCount: Exception {
	public readonly int expected;
	public readonly int got;

	public WrongArgCount(int expected, int got, bool atLeast, bool isContinuation)
	    : base("Wrong "+(isContinuation?"result":"argument")+
		   " count: expected "+(atLeast?"at least ":"")+
		   expected+", but got "+got)
	{
	    this.expected = expected;
	    this.got = got;
	}
    }
}

// Stolen from luna.sourceforge.net
//
// Original Copyright (C) Tom Berger 2003
// Modifications for Newmoon by Tony Garnock-Jones 2003
//
// Ported by Tom Berger from the Scheme48 reader by Richard Kelsey and
// Jonathan Rees.

using System;
using System.Text;
using System.IO;
using System.Collections;

namespace Newmoon {
    public class Reader {
	private delegate object ReadDispatchEntry(byte c, TextReader port);

	private class ReaderToken {
	    string Message;

	    public ReaderToken(string msg) {
		Message = msg;
	    }

	    public override string ToString() {
		return Message;
	    }	
	}

	private class ReadingException : Exception {
	    TextReader Port;
	    object[] Irritants;

	    public ReadingException(TextReader port, string msg, params object[] irritants)
		: base(msg)
	    {
		Port = port;
		Irritants = irritants;
	    }
	}

	private static byte AsciiLimit = 128;
	private static byte[] AsciiWhitespaces = {32, 10, 9, 12, 13};

	private static ReaderToken CloseParen = new ReaderToken("unexpected right parenthesis");
	private static ReaderToken Dot = new ReaderToken("unexpected \" . \"");

	private static ReadDispatchEntry[] ReadDispatchVector = new ReadDispatchEntry[AsciiLimit];
	private static bool[] IsReadTerminatingVector = new bool[AsciiLimit];
	private static Hashtable SharpMacros = new Hashtable();
	private static string[] StrangeSymbolNames = new string[] {"+", "-", "..."}; // %%%

	private Reader() {}

	static Reader() {
	    for (byte i = 0; i < AsciiLimit; i++) {
		ReadDispatchVector[i] = new ReadDispatchEntry(ReadDispatchEntryError);
		IsReadTerminatingVector[i] = true;
	    }

	    foreach (byte c in AsciiWhitespaces) {
		ReadDispatchVector[c] = new ReadDispatchEntry(SubReadWhitespace);
	    }

	    foreach (char c in "!$%&*+-./0123456789:<=>?@^_~ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz") {
		SetStandardSyntax(Convert.ToByte(c), false, new ReadDispatchEntry(SubReadConstituent));
	    }

	    SetStandardReadMacro('(', true, new ReadDispatchEntry(SubReadList));
	    SetStandardReadMacro(')', true, new ReadDispatchEntry(SubReadCloseParen));
	    SetStandardReadMacro('\'', true, new ReadDispatchEntry(SubReadQuote));
	    SetStandardReadMacro('`', true, new ReadDispatchEntry(SubReadQuasiquote));
	    SetStandardReadMacro(',', true, new ReadDispatchEntry(SubReadUnquote));
	    SetStandardReadMacro('"', true, new ReadDispatchEntry(SubReadString));
	    SetStandardReadMacro(';', true, new ReadDispatchEntry(SubReadComment));
	    SetStandardReadMacro('#', false, new ReadDispatchEntry(SubReadSharpSyntax));

	    DefineSharpMacro('f', new ReadDispatchEntry(FalseSharpMacro));
	    DefineSharpMacro('t', new ReadDispatchEntry(TrueSharpMacro));
	    DefineSharpMacro('\\', new ReadDispatchEntry(CharSharpMacro));
	    DefineSharpMacro('(', new ReadDispatchEntry(VectorSharpMacro));
	    DefineSharpMacro('\'', new ReadDispatchEntry(SubReadSyntax));
	    DefineSharpMacro('%', new ReadDispatchEntry(SubReadInternalSymbol));
	    DefineSharpMacro('0', new ReadDispatchEntry(SubReadSharing));
	    DefineSharpMacro('1', new ReadDispatchEntry(SubReadSharing));
	    DefineSharpMacro('2', new ReadDispatchEntry(SubReadSharing));
	    DefineSharpMacro('3', new ReadDispatchEntry(SubReadSharing));
	    DefineSharpMacro('4', new ReadDispatchEntry(SubReadSharing));
	    DefineSharpMacro('5', new ReadDispatchEntry(SubReadSharing));
	    DefineSharpMacro('6', new ReadDispatchEntry(SubReadSharing));
	    DefineSharpMacro('7', new ReadDispatchEntry(SubReadSharing));
	    DefineSharpMacro('8', new ReadDispatchEntry(SubReadSharing));
	    DefineSharpMacro('9', new ReadDispatchEntry(SubReadSharing));

	    foreach (char c in "bodxie") {
		DefineSharpMacro(c, new ReadDispatchEntry(NumberSharpMacro));
	    }
	}

	private static object ReadDispatchEntryError(byte c, TextReader port) {
	    throw new ReadingException(port, "illegal character read", c);
	}

	public static object Read(TextReader port) {
	    while (true) {
		sharing = new Hashtable();
		object Form = SubRead(port);
		if (!(Form is ReaderToken)) {
		    return Form;
		} else if (Form == CloseParen) {
		    // TOO MANY RIGHT PARENTHESIS
		    continue;
		} else {
		    throw new ReadingException(port, Form.ToString());
		}
	    }
	}

	private static object SubReadCarefully(TextReader port) {
	    object Form = SubRead(port);
	    if (Form == EofObject.EOF) {
		throw new ReadingException(port, "Unexpected End Of File");
	    } else if (Form is ReaderToken) {
		throw new ReadingException(port, Form.ToString());
	    } else {
		return Form;
	    }
	}

	private static object SubRead(TextReader port) {
	    int i = port.Read();
	    if (i < 0) {
		return EofObject.EOF;
	    } else {
		byte C = Convert.ToByte(i);
		return ReadDispatchVector[C](C, port);
	    }
	}

	private static void SetStandardSyntax(byte c, bool terminating, ReadDispatchEntry reader) {
	    ReadDispatchVector[c] = reader;
	    IsReadTerminatingVector[c] = terminating;
	}

	private static object SubReadWhitespace(byte c, TextReader port) {
	    return SubRead(port);
	}

	private static object SubReadConstituent(byte c, TextReader port) {
	    return ParseToken(SubReadToken(c, port), port);
	}

	private static void SetStandardReadMacro(char c, bool terminating, ReadDispatchEntry proc) {
	    SetStandardSyntax(Convert.ToByte(c), terminating, proc);
	}

	private static object SubReadListRecur(object form, TextReader port) {
	    if (form == EofObject.EOF) {
		throw new ReadingException(port, "end of file inside list -- unbalanced parentheses");
	    } else if (form == CloseParen) {
		return Null.NULL;
	    } else if (form == Dot) {
		object LastForm = SubReadCarefully(port);
		object AnotherForm = SubRead(port);
		if (AnotherForm == CloseParen) {
		    return LastForm;
		} else {
		    throw new ReadingException(port, "randomness after form after dot", AnotherForm);
		}
	    } else {
		return new Pair(form, SubReadListRecur(SubRead(port), port));
	    }
	}

	private static object SubReadList(byte c, TextReader port) {
	    object Form = SubRead(port);
	    if (Form == Dot) {
		throw new ReadingException(port, "missing car -- ( immediately followed by .");
	    } else {
		return SubReadListRecur(Form, port);
	    }
	}

	private static object SubReadCloseParen(byte c, TextReader port) {
	    return CloseParen;
	}

	private static object SubReadSyntax(byte c, TextReader port) {
	    return new Pair(String.Intern("syntax"), 
			    new Pair(SubReadCarefully(port), Null.NULL));
	}

	private static object SubReadInternalSymbol(byte c, TextReader port) {
	    port.Read();
	    return String.Intern("#%" + SubReadConstituent(Convert.ToByte(port.Read()), port));
	}

	private static Hashtable sharing = null;
	private static object SubReadSharing(byte c, TextReader port) {
	    StringBuilder b = new StringBuilder();
	    while (true) {
		char ch = Convert.ToChar(port.Read());
		if (Char.IsNumber(ch)) {
		    b.Append(ch);
		} else if (ch == '=') {
		    object o = SubReadCarefully(port);
		    sharing[b.ToString()] = o;
		    return o;
		} else if (ch == '#') {
		    return sharing[b.ToString()];
		} else {
		    throw new ReadingException(port, "illegal sharing terminator",
					       Convert.ToByte(ch));
		}
	    }
	}

	private static object SubReadQuote(byte c, TextReader port) {
	    return new Pair(String.Intern("quote"), 
			    new Pair(SubReadCarefully(port), Null.NULL));
	}

	private static object SubReadQuasiquote(byte c, TextReader port) {
	    return new Pair(String.Intern("quasiquote"), 
			    new Pair(SubReadCarefully(port), Null.NULL));
	}

	private static object SubReadUnquote(byte c, TextReader port) {
	    byte Next = Convert.ToByte(port.Peek());
	    if (Next < 0) {
		throw new ReadingException(port, "end of file after ,");
	    }
	    string Keyword;
	    if (Next == Convert.ToByte('@')) {
		port.Read();
		Keyword = "unquote-splicing";
	    } else {
		Keyword = "unquote";
	    }
	    return new Pair(Keyword, 
			    new Pair(SubReadCarefully(port), Null.NULL));
	}

	private static object SubReadString(byte c, TextReader port) {
	    StringBuilder Str = new StringBuilder();
	    while (true) {
		byte C = Convert.ToByte(port.Read());
		if (C < 0) {
		    throw new ReadingException(port, "end of file within a string");
		} else if (C == Convert.ToByte('\\')) {
		    int CC = port.Read();
		    switch (CC) {
		      case '\\':
		      case '"':
			  Str.Append(Convert.ToChar(CC));
			  continue;
		      case 'r': Str.Append(Convert.ToChar(13)); continue;
		      case 'n': Str.Append(Convert.ToChar(10)); continue;
		      case 't': Str.Append(Convert.ToChar(9)); continue;
		      case 'b': Str.Append(Convert.ToChar(8)); continue;
		      case 'a': Str.Append(Convert.ToChar(7)); continue;
		      default:
			  throw new ReadingException(port, "invalid escaped character in string", CC);
		    }
		} else if (C == Convert.ToByte('"')) {
		    break;
		} else {
		    Str.Append(Convert.ToChar(C));
		}
	    }
	    return new SchemeString(Str, false);
	}

	private static object SubReadComment(byte c, TextReader port) {
	    while(true) {
		int i = port.Read();
		if (i < 0) {
		    return EofObject.EOF;
		} else if (i == 10) {
		    return SubRead(port);
		} else {
		    continue;
		}
	    }
	}

	private static void DefineSharpMacro(char c, ReadDispatchEntry proc) {
	    SharpMacros[Convert.ToByte(c)] = proc;
	}

	private static object SubReadSharpSyntax(byte c, TextReader port) {
	    byte C = Convert.ToByte(port.Peek());
	    if (C < 0) {
		throw new ReadingException(port, "end of file after #");
	    } else {
		C = Convert.ToByte(char.ToLower(Convert.ToChar(C)));
	    }
	    object Reader = SharpMacros[C];
	    if (Reader is ReadDispatchEntry) {
		ReadDispatchEntry ReaderProc = (ReadDispatchEntry)Reader;
		return ReaderProc(C, port);
	    } else {
		throw new ReadingException(port, "unknown # syntax", C);
	    }
	}

	private static object FalseSharpMacro(byte c, TextReader port) {
	    port.Read();
	    return false;
	}

	private static object TrueSharpMacro(byte c, TextReader port) {
	    port.Read();
	    return true;
	}

	private static object CharSharpMacro(byte c, TextReader port) {
	    port.Read();
	    int C = port.Peek();

	    if (C < 0)
		throw new ReadingException(port, "end of file after #\\");

	    char CC = Convert.ToChar(C);

	    if (Char.IsLetter(CC)) {
		string Name = (string) SubReadCarefully(port);

		if (Name.Length == 1) {
		    return CC;
		} else if (Name == "space") {
		    return ' ';
		} else if (Name == "newline") {
		    return Convert.ToChar(10);
		} else {
		    throw new ReadingException(port, "unknown #\\ name ("+Name+")");
		}
	    }

	    port.Read();
	    return CC;
	}

	private static object VectorSharpMacro(byte c, TextReader port) {
	    port.Read();
	    List Lst = (List) SubReadList(c, port);
	    return Lst.ToVector();
	}

	private static object NumberSharpMacro(byte c, TextReader port) {
	    string str = SubReadToken(Convert.ToByte('#'), port);
	    try {
		// return Number.Parse(str); %%%
		return Int32.Parse(str); // %%%
	    } catch(Exception) {
		throw new ReadingException(port, "unsupported number syntax", str);
	    }
	}

	private static string SubReadToken(byte c, TextReader port) {
	    StringBuilder Token = new StringBuilder();
	    Token.Append(Convert.ToChar(c));
	    while (true) {
		byte C = Convert.ToByte(port.Peek());
		if ((C < 0) || IsReadTerminatingVector[C]) {
		    break;
		} else {
		    port.Read();
		    Token.Append(Char.ToLower(Convert.ToChar(C)));
		}
	    }
	    return Token.ToString();
	}

	private static object ParseToken(string str, TextReader port) {
	    byte c = Convert.ToByte(str[0]);
	    if (((c >= Convert.ToByte('0')) && (c <= Convert.ToByte('9'))) ||
		(c == Convert.ToByte('+')) ||
		(c == Convert.ToByte('-')) ||
		(c == Convert.ToByte('.'))) {
		try {
		    // return Number.Parse(str); %%%
		    return Int32.Parse(str); // %%%
		} catch(Exception) {
		    if (str == ".") {
			return Dot;
		    } else {
			return String.Intern(str); // %%%
		    }
		}   	   
	    } else {
		return String.Intern(str);
	    }
	}
    }
}

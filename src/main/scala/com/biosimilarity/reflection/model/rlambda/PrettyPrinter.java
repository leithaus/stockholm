package com.biosimilarity.reflection.model.rlambda;
import com.biosimilarity.reflection.model.rlambda.Absyn.*;

public class PrettyPrinter
{
  //For certain applications increasing the initial size of the buffer may improve performance.
  private static final int INITIAL_BUFFER_SIZE = 128;
  //You may wish to change the parentheses used in precedence.
  private static final String _L_PAREN = new String("(");
  private static final String _R_PAREN = new String(")");
  //You may wish to change render
  private static void render(String s)
  {
    if (s.equals("{"))
    {
       buf_.append("\n");
       indent();
       buf_.append(s);
       _n_ = _n_ + 2;
       buf_.append("\n");
       indent();
    }
    else if (s.equals("(") || s.equals("["))
       buf_.append(s);
    else if (s.equals(")") || s.equals("]"))
    {
       backup();
       buf_.append(s);
       buf_.append(" ");
    }
    else if (s.equals("}"))
    {
       _n_ = _n_ - 2;
       backup();
       backup();
       buf_.append(s);
       buf_.append("\n");
       indent();
    }
    else if (s.equals(","))
    {
       backup();
       buf_.append(s);
       buf_.append(" ");
    }
    else if (s.equals(";"))
    {
       backup();
       buf_.append(s);
       buf_.append("\n");
       indent();
    }
    else if (s.equals("")) return;
    else
    {
       buf_.append(s);
       buf_.append(" ");
    }
  }


  //  print and show methods are defined for each category.
  public static String print(com.biosimilarity.reflection.model.rlambda.Absyn.Expression foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.reflection.model.rlambda.Absyn.Expression foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.reflection.model.rlambda.Absyn.VariableExpr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.reflection.model.rlambda.Absyn.VariableExpr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.reflection.model.rlambda.Absyn.ValueExpr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.reflection.model.rlambda.Absyn.ValueExpr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.reflection.model.rlambda.Absyn.ListVariableExpr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.reflection.model.rlambda.Absyn.ListVariableExpr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  /***   You shouldn't need to change anything beyond this point.   ***/

  private static void pp(com.biosimilarity.reflection.model.rlambda.Absyn.Expression foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.reflection.model.rlambda.Absyn.Application)
    {
       com.biosimilarity.reflection.model.rlambda.Absyn.Application _application = (com.biosimilarity.reflection.model.rlambda.Absyn.Application) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_application.expression_1, 0);
       pp(_application.expression_2, 1);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.reflection.model.rlambda.Absyn.Mention)
    {
       com.biosimilarity.reflection.model.rlambda.Absyn.Mention _mention = (com.biosimilarity.reflection.model.rlambda.Absyn.Mention) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_mention.variableexpr_, 0);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.reflection.model.rlambda.Absyn.Value)
    {
       com.biosimilarity.reflection.model.rlambda.Absyn.Value _value = (com.biosimilarity.reflection.model.rlambda.Absyn.Value) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_value.valueexpr_, 0);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.reflection.model.rlambda.Absyn.Abstraction)
    {
       com.biosimilarity.reflection.model.rlambda.Absyn.Abstraction _abstraction = (com.biosimilarity.reflection.model.rlambda.Absyn.Abstraction) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("lambda");
       pp(_abstraction.listvariableexpr_, 0);
       render(".");
       pp(_abstraction.expression_, 1);
       if (_i_ > 1) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.reflection.model.rlambda.Absyn.VariableExpr foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.reflection.model.rlambda.Absyn.Transcription)
    {
       com.biosimilarity.reflection.model.rlambda.Absyn.Transcription _transcription = (com.biosimilarity.reflection.model.rlambda.Absyn.Transcription) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("@");
       render("<");
       pp(_transcription.expression_, 1);
       render(">");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.reflection.model.rlambda.Absyn.AtomLiteral)
    {
       com.biosimilarity.reflection.model.rlambda.Absyn.AtomLiteral _atomliteral = (com.biosimilarity.reflection.model.rlambda.Absyn.AtomLiteral) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_atomliteral.ident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.reflection.model.rlambda.Absyn.ValueExpr foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.reflection.model.rlambda.Absyn.Numeric)
    {
       com.biosimilarity.reflection.model.rlambda.Absyn.Numeric _numeric = (com.biosimilarity.reflection.model.rlambda.Absyn.Numeric) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_numeric.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.reflection.model.rlambda.Absyn.ListVariableExpr foo, int _i_)
  {
     for (java.util.Iterator<VariableExpr> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }


  private static void sh(com.biosimilarity.reflection.model.rlambda.Absyn.Expression foo)
  {
    if (foo instanceof com.biosimilarity.reflection.model.rlambda.Absyn.Application)
    {
       com.biosimilarity.reflection.model.rlambda.Absyn.Application _application = (com.biosimilarity.reflection.model.rlambda.Absyn.Application) foo;
       render("(");
       render("Application");
       sh(_application.expression_1);
       sh(_application.expression_2);
       render(")");
    }
    if (foo instanceof com.biosimilarity.reflection.model.rlambda.Absyn.Mention)
    {
       com.biosimilarity.reflection.model.rlambda.Absyn.Mention _mention = (com.biosimilarity.reflection.model.rlambda.Absyn.Mention) foo;
       render("(");
       render("Mention");
       sh(_mention.variableexpr_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.reflection.model.rlambda.Absyn.Value)
    {
       com.biosimilarity.reflection.model.rlambda.Absyn.Value _value = (com.biosimilarity.reflection.model.rlambda.Absyn.Value) foo;
       render("(");
       render("Value");
       sh(_value.valueexpr_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.reflection.model.rlambda.Absyn.Abstraction)
    {
       com.biosimilarity.reflection.model.rlambda.Absyn.Abstraction _abstraction = (com.biosimilarity.reflection.model.rlambda.Absyn.Abstraction) foo;
       render("(");
       render("Abstraction");
       render("[");
       sh(_abstraction.listvariableexpr_);
       render("]");
       sh(_abstraction.expression_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.reflection.model.rlambda.Absyn.VariableExpr foo)
  {
    if (foo instanceof com.biosimilarity.reflection.model.rlambda.Absyn.Transcription)
    {
       com.biosimilarity.reflection.model.rlambda.Absyn.Transcription _transcription = (com.biosimilarity.reflection.model.rlambda.Absyn.Transcription) foo;
       render("(");
       render("Transcription");
       sh(_transcription.expression_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.reflection.model.rlambda.Absyn.AtomLiteral)
    {
       com.biosimilarity.reflection.model.rlambda.Absyn.AtomLiteral _atomliteral = (com.biosimilarity.reflection.model.rlambda.Absyn.AtomLiteral) foo;
       render("(");
       render("AtomLiteral");
       sh(_atomliteral.ident_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.reflection.model.rlambda.Absyn.ValueExpr foo)
  {
    if (foo instanceof com.biosimilarity.reflection.model.rlambda.Absyn.Numeric)
    {
       com.biosimilarity.reflection.model.rlambda.Absyn.Numeric _numeric = (com.biosimilarity.reflection.model.rlambda.Absyn.Numeric) foo;
       render("(");
       render("Numeric");
       sh(_numeric.integer_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.reflection.model.rlambda.Absyn.ListVariableExpr foo)
  {
     for (java.util.Iterator<VariableExpr> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }


  private static void pp(Integer n, int _i_) { buf_.append(n); buf_.append(" "); }
  private static void pp(Double d, int _i_) { buf_.append(d); buf_.append(" "); }
  private static void pp(String s, int _i_) { buf_.append(s); buf_.append(" "); }
  private static void pp(Character c, int _i_) { buf_.append("'" + c.toString() + "'"); buf_.append(" "); }
  private static void sh(Integer n) { render(n.toString()); }
  private static void sh(Double d) { render(d.toString()); }
  private static void sh(Character c) { render(c.toString()); }
  private static void sh(String s) { printQuoted(s); }
  private static void printQuoted(String s) { render("\"" + s + "\""); }
  private static void indent()
  {
    int n = _n_;
    while (n > 0)
    {
      buf_.append(" ");
      n--;
    }
  }
  private static void backup()
  {
     if (buf_.charAt(buf_.length() - 1) == ' ') {
      buf_.setLength(buf_.length() - 1);
    }
  }
  private static void trim()
  {
     while (buf_.length() > 0 && buf_.charAt(0) == ' ')
        buf_.deleteCharAt(0); 
    while (buf_.length() > 0 && buf_.charAt(buf_.length()-1) == ' ')
        buf_.deleteCharAt(buf_.length()-1);
  }
  private static int _n_ = 0;
  private static StringBuilder buf_ = new StringBuilder(INITIAL_BUFFER_SIZE);
}


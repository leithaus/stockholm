package com.biosimilarity.reflection.model.rlambda;
import com.biosimilarity.reflection.model.rlambda.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  com.biosimilarity.reflection.model.rlambda.Absyn.Expression.Visitor<com.biosimilarity.reflection.model.rlambda.Absyn.Expression,A>,
  com.biosimilarity.reflection.model.rlambda.Absyn.VariableExpr.Visitor<com.biosimilarity.reflection.model.rlambda.Absyn.VariableExpr,A>,
  com.biosimilarity.reflection.model.rlambda.Absyn.ValueExpr.Visitor<com.biosimilarity.reflection.model.rlambda.Absyn.ValueExpr,A>
{
/* Expression */
    public Expression visit(com.biosimilarity.reflection.model.rlambda.Absyn.Application p, A arg)
    {
      Expression expression_1 = p.expression_1.accept(this, arg);
      Expression expression_2 = p.expression_2.accept(this, arg);

      return new com.biosimilarity.reflection.model.rlambda.Absyn.Application(expression_1, expression_2);
    }
    public Expression visit(com.biosimilarity.reflection.model.rlambda.Absyn.Mention p, A arg)
    {
      VariableExpr variableexpr_ = p.variableexpr_.accept(this, arg);

      return new com.biosimilarity.reflection.model.rlambda.Absyn.Mention(variableexpr_);
    }
    public Expression visit(com.biosimilarity.reflection.model.rlambda.Absyn.Value p, A arg)
    {
      ValueExpr valueexpr_ = p.valueexpr_.accept(this, arg);

      return new com.biosimilarity.reflection.model.rlambda.Absyn.Value(valueexpr_);
    }
    public Expression visit(com.biosimilarity.reflection.model.rlambda.Absyn.Abstraction p, A arg)
    {
      ListVariableExpr listvariableexpr_ = new ListVariableExpr();
      for (VariableExpr x : p.listvariableexpr_) {
        listvariableexpr_.add(x.accept(this,arg));
      }
      Expression expression_ = p.expression_.accept(this, arg);

      return new com.biosimilarity.reflection.model.rlambda.Absyn.Abstraction(listvariableexpr_, expression_);
    }

/* VariableExpr */
    public VariableExpr visit(com.biosimilarity.reflection.model.rlambda.Absyn.Transcription p, A arg)
    {
      Expression expression_ = p.expression_.accept(this, arg);

      return new com.biosimilarity.reflection.model.rlambda.Absyn.Transcription(expression_);
    }
    public VariableExpr visit(com.biosimilarity.reflection.model.rlambda.Absyn.AtomLiteral p, A arg)
    {
      String ident_ = p.ident_;

      return new com.biosimilarity.reflection.model.rlambda.Absyn.AtomLiteral(ident_);
    }

/* ValueExpr */
    public ValueExpr visit(com.biosimilarity.reflection.model.rlambda.Absyn.Numeric p, A arg)
    {
      Integer integer_ = p.integer_;

      return new com.biosimilarity.reflection.model.rlambda.Absyn.Numeric(integer_);
    }

}
package com.biosimilarity.reflection.model.rlambda.Absyn; // Java Package generated by the BNF Converter.

public class Value extends Expression {
  public final ValueExpr valueexpr_;

  public Value(ValueExpr p1) { valueexpr_ = p1; }

  public <R,A> R accept(com.biosimilarity.reflection.model.rlambda.Absyn.Expression.Visitor<R,A> v, A arg) { return v.visit(this, arg); }

  public boolean equals(Object o) {
    if (this == o) return true;
    if (o instanceof com.biosimilarity.reflection.model.rlambda.Absyn.Value) {
      com.biosimilarity.reflection.model.rlambda.Absyn.Value x = (com.biosimilarity.reflection.model.rlambda.Absyn.Value)o;
      return this.valueexpr_.equals(x.valueexpr_);
    }
    return false;
  }

  public int hashCode() {
    return this.valueexpr_.hashCode();
  }


}
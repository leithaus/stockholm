package com.biosimilarity.reflection.model.rlambda.Absyn; // Java Package generated by the BNF Converter.

public class Transcription extends VariableExpr {
  public final Expression expression_;

  public Transcription(Expression p1) { expression_ = p1; }

  public <R,A> R accept(com.biosimilarity.reflection.model.rlambda.Absyn.VariableExpr.Visitor<R,A> v, A arg) { return v.visit(this, arg); }

  public boolean equals(Object o) {
    if (this == o) return true;
    if (o instanceof com.biosimilarity.reflection.model.rlambda.Absyn.Transcription) {
      com.biosimilarity.reflection.model.rlambda.Absyn.Transcription x = (com.biosimilarity.reflection.model.rlambda.Absyn.Transcription)o;
      return this.expression_.equals(x.expression_);
    }
    return false;
  }

  public int hashCode() {
    return this.expression_.hashCode();
  }


}
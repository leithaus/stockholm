package com.biosimilarity.reflection.lib.ddl.tbldecl; // Java Package generated by the BNF Converter.

public class TblClassName extends TableClassName {
  public final String ident_;

  public TblClassName(String p1) { ident_ = p1; }

  public <R,A> R accept(com.biosimilarity.reflection.lib.ddl.tbldecl.TableClassName.Visitor<R,A> v, A arg) { return v.visit(this, arg); }

  public boolean equals(Object o) {
    if (this == o) return true;
    if (o instanceof com.biosimilarity.reflection.lib.ddl.tbldecl.TblClassName) {
      com.biosimilarity.reflection.lib.ddl.tbldecl.TblClassName x = (com.biosimilarity.reflection.lib.ddl.tbldecl.TblClassName)o;
      return this.ident_.equals(x.ident_);
    }
    return false;
  }

  public int hashCode() {
    return this.ident_.hashCode();
  }


}

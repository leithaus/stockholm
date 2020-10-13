package com.biosimilarity.reflection.lib.ddl.tbldecl; // Java Package generated by the BNF Converter.

public class TblNamedType extends TableCompositeType {
  public final String ident_;

  public TblNamedType(String p1) { ident_ = p1; }

  public <R,A> R accept(com.biosimilarity.reflection.lib.ddl.tbldecl.TableCompositeType.Visitor<R,A> v, A arg) { return v.visit(this, arg); }

  public boolean equals(Object o) {
    if (this == o) return true;
    if (o instanceof com.biosimilarity.reflection.lib.ddl.tbldecl.TblNamedType) {
      com.biosimilarity.reflection.lib.ddl.tbldecl.TblNamedType x = (com.biosimilarity.reflection.lib.ddl.tbldecl.TblNamedType)o;
      return this.ident_.equals(x.ident_);
    }
    return false;
  }

  public int hashCode() {
    return this.ident_.hashCode();
  }


}
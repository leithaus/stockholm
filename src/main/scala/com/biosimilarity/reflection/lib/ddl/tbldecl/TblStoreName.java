package com.biosimilarity.reflection.lib.ddl.tbldecl; // Java Package generated by the BNF Converter.

public class TblStoreName extends TableStoreName {
  public final String string_;

  public TblStoreName(String p1) { string_ = p1; }

  public <R,A> R accept(com.biosimilarity.reflection.lib.ddl.tbldecl.TableStoreName.Visitor<R,A> v, A arg) { return v.visit(this, arg); }

  public boolean equals(Object o) {
    if (this == o) return true;
    if (o instanceof com.biosimilarity.reflection.lib.ddl.tbldecl.TblStoreName) {
      com.biosimilarity.reflection.lib.ddl.tbldecl.TblStoreName x = (com.biosimilarity.reflection.lib.ddl.tbldecl.TblStoreName)o;
      return this.string_.equals(x.string_);
    }
    return false;
  }

  public int hashCode() {
    return this.string_.hashCode();
  }


}

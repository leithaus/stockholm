package com.biosimilarity.reflection.lib.ddl.tbldecl; // Java Package generated by the BNF Converter.

public class TblGrndTypeBool extends TableGroundType {

  public TblGrndTypeBool() { }

  public <R,A> R accept(com.biosimilarity.reflection.lib.ddl.tbldecl.TableGroundType.Visitor<R,A> v, A arg) { return v.visit(this, arg); }

  public boolean equals(Object o) {
    if (this == o) return true;
    if (o instanceof com.biosimilarity.reflection.lib.ddl.tbldecl.TblGrndTypeBool) {
      return true;
    }
    return false;
  }

  public int hashCode() {
    return 37;
  }


}

package com.biosimilarity.reflection.lib.ddl.tbldecl; // Java Package generated by the BNF Converter.

public class TblDefaultValBool extends TableDefaultValue {
  public final TableBoolean tableboolean_;

  public TblDefaultValBool(TableBoolean p1) { tableboolean_ = p1; }

  public <R,A> R accept(com.biosimilarity.reflection.lib.ddl.tbldecl.TableDefaultValue.Visitor<R,A> v, A arg) { return v.visit(this, arg); }

  public boolean equals(Object o) {
    if (this == o) return true;
    if (o instanceof com.biosimilarity.reflection.lib.ddl.tbldecl.TblDefaultValBool) {
      com.biosimilarity.reflection.lib.ddl.tbldecl.TblDefaultValBool x = (com.biosimilarity.reflection.lib.ddl.tbldecl.TblDefaultValBool)o;
      return this.tableboolean_.equals(x.tableboolean_);
    }
    return false;
  }

  public int hashCode() {
    return this.tableboolean_.hashCode();
  }


}

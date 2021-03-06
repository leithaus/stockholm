package com.biosimilarity.reflection.lib.ddl.tbldecl; // Java Package generated by the BNF Converter.

public abstract class TableDefaultValue implements java.io.Serializable {
  public abstract <R,A> R accept(TableDefaultValue.Visitor<R,A> v, A arg);
  public interface Visitor <R,A> {
    public R visit(com.biosimilarity.reflection.lib.ddl.tbldecl.TblDefaultValBool p, A arg);
    public R visit(com.biosimilarity.reflection.lib.ddl.tbldecl.TblDefaultValInt p, A arg);
    public R visit(com.biosimilarity.reflection.lib.ddl.tbldecl.TblDefaultValStr p, A arg);

  }

}

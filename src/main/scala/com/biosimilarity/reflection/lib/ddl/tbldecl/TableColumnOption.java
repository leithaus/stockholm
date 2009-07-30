package com.biosimilarity.reflection.lib.ddl.tbldecl; // Java Package generated by the BNF Converter.

public abstract class TableColumnOption implements java.io.Serializable {
  public abstract <R,A> R accept(TableColumnOption.Visitor<R,A> v, A arg);
  public interface Visitor <R,A> {
    public R visit(com.biosimilarity.reflection.lib.ddl.tbldecl.TblColumnAutoInc p, A arg);
    public R visit(com.biosimilarity.reflection.lib.ddl.tbldecl.TblColumnNotNull p, A arg);
    public R visit(com.biosimilarity.reflection.lib.ddl.tbldecl.TblColumnDefault p, A arg);

  }

}

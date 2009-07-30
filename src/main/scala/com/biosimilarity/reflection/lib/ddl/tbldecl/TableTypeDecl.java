package com.biosimilarity.reflection.lib.ddl.tbldecl; // Java Package generated by the BNF Converter.

public abstract class TableTypeDecl implements java.io.Serializable {
  public abstract <R,A> R accept(TableTypeDecl.Visitor<R,A> v, A arg);
  public interface Visitor <R,A> {
    public R visit(com.biosimilarity.reflection.lib.ddl.tbldecl.TblGroundType p, A arg);
    public R visit(com.biosimilarity.reflection.lib.ddl.tbldecl.TblCompType p, A arg);

  }

}

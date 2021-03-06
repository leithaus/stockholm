package com.biosimilarity.reflection.lib.ddl.tbldecl; // Java Package generated by the BNF Converter.

public class TblDecl extends TableDecl {
  public final String ident_1, string_2;
  public final ListTableTypeDecl listtabletypedecl_;
  public final ListTableMthdDecl listtablemthddecl_;

  public TblDecl(String p1, ListTableTypeDecl p2, String p3, ListTableMthdDecl p4) { ident_1 = p1; listtabletypedecl_ = p2; string_2 = p3; listtablemthddecl_ = p4; }

  public <R,A> R accept(com.biosimilarity.reflection.lib.ddl.tbldecl.TableDecl.Visitor<R,A> v, A arg) { return v.visit(this, arg); }

  public boolean equals(Object o) {
    if (this == o) return true;
    if (o instanceof com.biosimilarity.reflection.lib.ddl.tbldecl.TblDecl) {
      com.biosimilarity.reflection.lib.ddl.tbldecl.TblDecl x = (com.biosimilarity.reflection.lib.ddl.tbldecl.TblDecl)o;
      return this.ident_1.equals(x.ident_1) && this.listtabletypedecl_.equals(x.listtabletypedecl_) && this.string_2.equals(x.string_2) && this.listtablemthddecl_.equals(x.listtablemthddecl_);
    }
    return false;
  }

  public int hashCode() {
    return 37*(37*(37*(this.ident_1.hashCode())+this.listtabletypedecl_.hashCode())+this.string_2.hashCode())+this.listtablemthddecl_.hashCode();
  }


}

// -*- mode: Scala;-*- 
// Filename:    Iago.scala 
// Authors:     lgm                                                    
// Creation:    Thu Feb 26 14:37:29 2009 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.reflection.lib.othello;    

import japa.parser._
import japa.parser.ast._
import japa.parser.ast.body._
import japa.parser.ast.stmt._
import japa.parser.ast.expr._
import japa.parser.ast.`type`._
import japa.parser.ast.visitor._

import java.io.FileInputStream

trait Desdemona {
  case class SourceTransformer(
    projectName : String,
    location : String,
    store : String
  ) {
    val _trgtPackageDependencies : List[String] =
      List(
	"javax.persistence.CascadeType",
	"javax.persistence.Column",
	"javax.persistence.Entity",
	"javax.persistence.FetchType",
	"javax.persistence.Id",
	"javax.persistence.OneToMany",
	"javax.persistence.Table",
	"javax.persistence.UniqueConstraint",	
	"javax.persistence.MappedSuperclass",	
	"javax.persistence.GeneratedValue",	
	"javax.persistence.Inheritance",	
	"javax.persistence.InheritanceType",	
	"org.hibernate.annotations.GenericGenerator",
	"java.util.Date",
	"java.util.HashSet",
	"java.util.Set",
	"java.util.List",
	"java.util.Iterator",
	"java.net.URI"	
	)

    def trgtPackageDependencies : List[String] = {
      _trgtPackageDependencies 
    }

    var _srcCompilationUnit : Option[CompilationUnit] = None
    var _trgtCompilationUnit : Option[CompilationUnit] = None
    
    def pathSeparator : String = "/"

    def modelMemoFieldName : String = "_model"

    def srcLocationToStream() : FileInputStream = {
      new FileInputStream( location )
    }
    def srcPackageName : String = {
      srcCompilationUnit.getPackage.getName.toString
    }
    def trgtPackageSuffix : String = ".persistence.sql"
    def trgtPackageName : String = {
      srcPackageName + trgtPackageSuffix
    }
    def trgtRootClassName : String = resourceModelName
    def trgtRootClass : ClassOrInterfaceType = {
      new ClassOrInterfaceType( trgtRootClassName )
    }
    def trgtStoreName : String = store
    def trgtTableName : String = resourceModelName + "_table"
    def trgtUniqueIdFldName : String = "uuid"
    def trgtIdFldName : String = "id"

    def srcCompilationUnit : CompilationUnit = {
      _srcCompilationUnit match {
	case Some( sCU ) => sCU
	case None => {
	  var srcStream : FileInputStream = null;
	  try {
	    srcStream = srcLocationToStream()
	    val sCU = JavaParser.parse( srcStream );
	    _srcCompilationUnit = Some( sCU );
	    sCU
	  }
	  finally {
	    srcStream.close
	  }
	}
      }
    }
    def trgtCompilationUnit : CompilationUnit = {
      _trgtCompilationUnit match {
	case Some( tCU ) => tCU
	case None => {
	  val tCU = new CompilationUnit() ;
	  _trgtCompilationUnit = Some( tCU );
	  tCU
	}
      }
    }
    def isContainer() : Boolean = {
      true
    }
    def trgtRenderType(
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration,
      member : FieldDeclaration
      ) : Type = {
	val ftypName =
	  member.getType match {
	    case rType : ReferenceType => {
	      rType.getType.asInstanceOf[ClassOrInterfaceType].getName
	    }
	    case cOIType : ClassOrInterfaceType => {
	      cOIType.getName
	    }
	    case _ => {
	      throw new Exception( "class type not yet handled" )
	    }
	  };
		 
	if ( (ftypName.length > 4)
	    && (ftypName.substring( 0, 4 ) == "List") ) {
	      val theTypeParamStr : String =
		ftypName.substring( 4, ftypName.length );
	      val theTypeParamType : ClassOrInterfaceType =
		new ClassOrInterfaceType( theTypeParamStr );
	      val theType : ClassOrInterfaceType =
		new ClassOrInterfaceType( "List" );
	      theType.setTypeArgs( new java.util.LinkedList[Type]() );
	      theType.getTypeArgs.add( theTypeParamType  )
	      theType
	    }
	else {
	  // BUGBUG lgm should make copy
	  member.getType
	}
      }
    def trgtRenderMethodName : String = "render"

    def resourceIsConcrete : Boolean = {
      !( ModifierSet.isAbstract( resourceModelDecl.getModifiers ) )
    }

    def resourceModelDecl : TypeDeclaration = {
      srcCompilationUnit.getTypes.get(0)
    }
    def resourceModelName : String = {
      resourceModelDecl.getName.toString
    }
    def resourceModelNameExpr : NameExpr = {
      // ensuring fresh name expr
      ASTHelper.createNameExpr( resourceModelName )
    }
    def resourceModelCtors()
    : Seq[ConstructorDeclaration] = {
      for (member <-
	   scala.collection.jcl.Conversions.convertList(
	     srcCompilationUnit.getTypes.get(0).getMembers
	   ) if (member.isInstanceOf[japa.parser.ast.body.ConstructorDeclaration]))
      yield member.asInstanceOf[japa.parser.ast.body.ConstructorDeclaration]
    }
    def resourceModelQualifiedCtor( qualification : String )
    : Option[ConstructorDeclaration] = {
      val ctorMatches : Seq[ConstructorDeclaration] =
	(for (ctor <-
	      (for (member <-
		    scala.collection.jcl.Conversions.convertList(
		      srcCompilationUnit.getTypes.get(0).getMembers
		    ) if (member.isInstanceOf[japa.parser.ast.body.ConstructorDeclaration]))
	       yield member.asInstanceOf[japa.parser.ast.body.ConstructorDeclaration])
	      if ( (ctor.getJavaDoc != null) && (ctor.getJavaDoc.toString.contains( qualification )) ))
	       yield ctor);
      ctorMatches.length match {
	case 0 => None
	case _ => Some( ctorMatches( 0 ) )
      }
    }    
    def resourceModelMinimalCtor : Option[ConstructorDeclaration] =
      resourceModelQualifiedCtor( "minimal" )
    def resourceModelFullCtor : Option[ConstructorDeclaration] =
      resourceModelQualifiedCtor( "full" )
    def resourceModelMinimalCtorParams : List[Parameter] = {
      resourceModelMinimalCtor match {
	case None => List()
	case Some( mCtor ) => {
	  mCtor.getParameters match {
	    case null => List()
	    case mCtorParams =>
	    ( (List() : List[Parameter]) /:
	       scala.collection.jcl.Conversions.convertList(
		 mCtorParams
	       ) )({
		 ( acc : List[Parameter], param : Parameter ) => {
		   // BUGBUG lgm should copy type and id
		   val nParam : Parameter =
		     new Parameter( param.getType, param.getId );
		   nParam.setAnnotations(
		     new java.util.LinkedList[AnnotationExpr]()
		   );
		   nParam.getAnnotations.add(
		     new SingleMemberAnnotationExpr(
		       ASTHelper.createNameExpr( "QueryParam" ),
		       new StringLiteralExpr( param.getId.toString )
		     )
		   );
		   
		   acc ::: List( nParam )
		 }
	       })
	  }
	}
      }
    }
    def trgtResourceClassName : String = {
      ( resourceModelName + "Resource")
    }
    def trgtImportDecls( cUnit : CompilationUnit )
    : CompilationUnit = {
      val importDecls : java.util.List[ImportDeclaration] =
	(new java.util.LinkedList[ImportDeclaration]() /:
	 //(trgtPackageDependencies ::: List( srcPackageName + ".*", trgtPackageName + ".*" ))
	 (trgtPackageDependencies ::: List( trgtPackageName + ".*" )))(
	  { (acc : java.util.LinkedList[ImportDeclaration], pkgName : String) =>
	    acc.add(
	      new ImportDeclaration(
		ASTHelper.createNameExpr( pkgName ),
		false,
		false
	      )
	    ); 
	   acc
	 }
	)
      cUnit.setImports(
	importDecls
      );
      cUnit
    }
    def addSQLPackage( cUnit : CompilationUnit )
    : CompilationUnit = {
      cUnit.setPackage(
	new PackageDeclaration(
	  ASTHelper.createNameExpr( srcPackageName + trgtPackageSuffix )
	)
      );
      cUnit
    }

    def makeScope( spath : String ) : ClassOrInterfaceType = {
      val root :: path = spath.split( "\\." ).toList;
      ( new ClassOrInterfaceType( root ) /: path )( {
	( acc : ClassOrInterfaceType, name : String ) => {
	  new ClassOrInterfaceType( acc, name )
	}
      } )
    }

    def resourceScope : ClassOrInterfaceType = {
      makeScope( srcPackageName )
    }

    class PackageCorrectionVisitor
    extends VoidVisitorAdapter[scala.collection.mutable.Map[String,String]] {      
      override def visit(
	param : Parameter,
	arg : scala.collection.mutable.Map[String,String]
	) = {
	  param.getType match {
	    case cOrIType : ClassOrInterfaceType =>
	      visit(
		param.getType.asInstanceOf[ClassOrInterfaceType],
		arg
	      )
	    case refType : ReferenceType =>
	      visit(
		param.getType.asInstanceOf[ReferenceType],
		arg
	      )
	  }	      
        }

      override def visit(
	cOrIType : ClassOrInterfaceType,
	arg : scala.collection.mutable.Map[String,String]
	) = {
	  var scope = cOrIType.getScope;
	  if (scope != null) {
	    if (scope.toString == srcPackageName) {
		cOrIType.setScope(
		  new ClassOrInterfaceType( 
		    new ClassOrInterfaceType( 
		      scope,
		      "persistence"
		    ),
		    "sql"
		  )
		)
	    }
	    else {
	      visit( scope, arg )
	    }
	  }
        }
      override def visit(
	refType : ReferenceType,
	arg : scala.collection.mutable.Map[String,String]
	) = {
	  val cOrIType : ClassOrInterfaceType =
	    refType.getType.asInstanceOf[ClassOrInterfaceType];
	  visit( cOrIType, arg )
	}
    }

    var _packageCorrectionVisitor : Option[PackageCorrectionVisitor] =
      None
    def packageCorrectionVisitor : PackageCorrectionVisitor = {
      _packageCorrectionVisitor match {
	case Some( pcv ) => pcv
	case None => {
	  val pcv = new PackageCorrectionVisitor()
	  _packageCorrectionVisitor = Some( pcv )
	  pcv
	}
      }
    }
    var _packageMap : Option[scala.collection.mutable.Map[String,String]] 
    = None
    def packageMap : scala.collection.mutable.Map[String,String] = {
      _packageMap match {
	case Some( pkgm ) => pkgm
	case None => {
	  val pkgm =
	    new scala.collection.mutable.HashMap[String,String]();
	  _packageMap = Some( pkgm )
	  pkgm
	}
      }
    }

    def correctMemberPackageReferences(
      typ : ClassOrInterfaceDeclaration
    )
    : ClassOrInterfaceDeclaration = {
      packageCorrectionVisitor.visit( typ, packageMap );
      typ
    }

    def addSQLResourceClass( cUnit : CompilationUnit )
    : ( CompilationUnit, ClassOrInterfaceDeclaration ) = {
      val typ : ClassOrInterfaceDeclaration = resourceModelDecl.asInstanceOf[ClassOrInterfaceDeclaration]
	// new ClassOrInterfaceDeclaration(
// 	  ModifierSet.PUBLIC,
// 	  false,
// 	  trgtResourceClassName
// 	);
      ASTHelper.addTypeDeclaration( cUnit, typ );
      if ((resourceIsConcrete) && (!isCollectionType( typ ))) {	
	typ.setAnnotations( new java.util.LinkedList[AnnotationExpr]() );
	typ.getAnnotations.add(
	  new MarkerAnnotationExpr(
	    ASTHelper.createNameExpr( "Entity" )
	  )
	);

	val inheritanceAnnotationDecl : NormalAnnotationExpr =
	  new NormalAnnotationExpr(
	    ASTHelper.createNameExpr( "Inheritance" ),
	    new java.util.LinkedList[MemberValuePair]()
	  );
	inheritanceAnnotationDecl.getPairs().add(
	  new MemberValuePair(
	    "strategy", 
	    new NameExpr(
	      "InheritanceType.TABLE_PER_CLASS"
	    )
	  )
	);
	typ.getAnnotations.add( inheritanceAnnotationDecl );
	
	val tableAnnotationDecl : NormalAnnotationExpr =
	  new NormalAnnotationExpr(
	    ASTHelper.createNameExpr( "Table" ),
	    new java.util.LinkedList[MemberValuePair]()
	  );
	tableAnnotationDecl.getPairs().add(
	  new MemberValuePair(
	    "name", 
	    new StringLiteralExpr( trgtTableName )
	  )
	);
	tableAnnotationDecl.getPairs().add(
	  new MemberValuePair(
	    "catalog", 
	    new StringLiteralExpr( trgtStoreName )
	  )
	);
	val uniqueConstraintsDecl : ArrayInitializerExpr =
	  new ArrayInitializerExpr(
	    new java.util.LinkedList[Expression]()
	  );
	val constraint : NormalAnnotationExpr =
	  new NormalAnnotationExpr(
	    new NameExpr( "UniqueConstraint" ),
	    new java.util.LinkedList[MemberValuePair]()
	  );
	
	constraint.getPairs().add(
	  new MemberValuePair( 
	    "columnNames",
	    new StringLiteralExpr( trgtUniqueIdFldName )
	  )
	);
	
	uniqueConstraintsDecl.getValues().add( constraint );
	
	tableAnnotationDecl.getPairs().add(
	  new MemberValuePair(
	    "uniqueConstraints",
	    uniqueConstraintsDecl
	  )
	);
	typ.getAnnotations.add( tableAnnotationDecl );
	//typ.setExtends( new java.util.LinkedList[ClassOrInterfaceType]() );
	//typ.getExtends.add( trgtRootClass );      
      }
      else if (!resourceIsConcrete) {
	typ.setAnnotations( new java.util.LinkedList[AnnotationExpr]() );
	typ.getAnnotations.add(
	  new MarkerAnnotationExpr(
	    //ASTHelper.createNameExpr( "MappedSuperclass" )
	    ASTHelper.createNameExpr( "Entity" )
	  )
	);
	val inheritanceAnnotationDecl : NormalAnnotationExpr =
	  new NormalAnnotationExpr(
	    ASTHelper.createNameExpr( "Inheritance" ),
	    new java.util.LinkedList[MemberValuePair]()
	  );
	inheritanceAnnotationDecl.getPairs().add(
	  new MemberValuePair(
	    "strategy", 
	    new NameExpr(
	      "InheritanceType.TABLE_PER_CLASS"
	    )
	  )
	);
	typ.getAnnotations.add( inheritanceAnnotationDecl );

	val idField : FieldDeclaration =
	ASTHelper.createFieldDeclaration(
	  ModifierSet.PRIVATE,
	  new ClassOrInterfaceType( "String" ),
	  trgtIdFldName + "Super"
	);
	idField.setAnnotations(
	  new java.util.LinkedList[AnnotationExpr]()
	);
	idField.getAnnotations.add(
	  new MarkerAnnotationExpr(
	    ASTHelper.createNameExpr( "Id" )
	  )
	)
	// idField.getAnnotations.add(
// 	  new MarkerAnnotationExpr(
// 	    ASTHelper.createNameExpr( "GeneratedValue" )
// 	  )
// 	)
// 	val generatorAnnotationDecl : NormalAnnotationExpr =
// 	  new NormalAnnotationExpr(
// 	    ASTHelper.createNameExpr( "GenericGenerator" ),
// 	    new java.util.LinkedList[MemberValuePair]()
// 	  );
// 	generatorAnnotationDecl.getPairs().add(
// 	  new MemberValuePair(
// 	    "name", 
// 	    new StringLiteralExpr( "hibernate-uuid" )
// 	  )
// 	);
// 	generatorAnnotationDecl.getPairs().add(
// 	  new MemberValuePair(
// 	    "strategy", 
// 	    new StringLiteralExpr( "uuid" )
// 	  )
// 	);
// 	idField.getAnnotations.add( 
// 	  generatorAnnotationDecl
// 	);

	ASTHelper.addMember(typ, idField);
	
      }

      correctMemberPackageReferences( typ );
      
      (	cUnit, typ )
    }
    def addSQLResourceCtxtFields(
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration
    )
    : CompilationUnit = {
      val uuidField : FieldDeclaration =
	ASTHelper.createFieldDeclaration(
	  ModifierSet.PRIVATE,
	  new ClassOrInterfaceType( "String" ),
	  trgtUniqueIdFldName
	);
      val idField : FieldDeclaration =
	ASTHelper.createFieldDeclaration(
	  ModifierSet.PRIVATE,
	  new ClassOrInterfaceType( "String" ),
	  trgtIdFldName
	);
      ASTHelper.addMember(typ, uuidField);
      ASTHelper.addMember(typ, idField);
      cUnit
    }
    def addSQLResourceCtor(
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration
    )
    : CompilationUnit = {
      // def getFirstId( fd : FieldDeclaration ) : VariableDeclaratorId = {
// 	(scala.collection.jcl.Conversions.convertList(
// 	  fd.getVariables
// 	  ))( 0 ).getId
//       }
//       def getIds( fd : FieldDeclaration ) : Seq[VariableDeclaratorId] = {
// 	(scala.collection.jcl.Conversions.convertList(
// 	  fd.getVariables
// 	  )).map( { ( v : VariableDeclarator ) => v.getId } )
//       }

//       val ctor : ConstructorDeclaration =
// 	new ConstructorDeclaration( 
// 	  ModifierSet.PUBLIC,
// 	  //trgtResourceClassName
// 	  typ.getName
// 	);

//       val block : BlockStmt = new BlockStmt();
//       val callModel : ExplicitConstructorInvocationStmt =	      
// 	new ExplicitConstructorInvocationStmt(
// 	  false,
// 	  null,
// 	  new java.util.LinkedList[Expression]()
// 	);      
//       ASTHelper.addStmt( block, callModel );      
      
//       ctor.setParameters( new java.util.LinkedList[Parameter]() )
//       for (
// 	fieldDecl <- getContainedFields;
// 	mparams =
// 	  getIds( fieldDecl ).map({ ( id : VariableDeclaratorId ) =>
// 	    new Parameter(
// 	      fieldDecl.getType,
// 	      id
// 	    )
// 	  })
//       )
// 	yield {
// 	  mparams.map( { ( mparam : Parameter ) => {
// 	    ctor.getParameters.add( mparam );
// 	    callModel.getArgs.add(
// 	      new NameExpr( mparam.getId.toString )
// 	    )
// 	  } 
// 	 })
// 	};

//       ASTHelper.addMember( typ, ctor );
//       ctor.setBlock( block );
      cUnit
    }

    def isCollectionType( typ : TypeDeclaration ) : Boolean = {
      val ftypName = typ.getName;
      
      ((ftypName.length > 4)
       && (ftypName.substring( 0, 4 ) == "List") ) 
    }

    def addColumnAnnotations(
      methodDecl : MethodDeclaration,
      fieldDecl : FieldDeclaration,
      nameStem : String,
      annotations : java.util.LinkedList[AnnotationExpr]
      ) : MethodDeclaration = {
	val cPairs = new java.util.LinkedList[MemberValuePair]();
	val columnAnnotation : NormalAnnotationExpr =
	  new NormalAnnotationExpr(
	    null,
	    cPairs
	  );
	// BUGBUG -- LGM might not be this kind of type
	val ftypName =
	  fieldDecl.getType match {
	    case rType : ReferenceType => {
	      rType.getType.asInstanceOf[ClassOrInterfaceType].getName
	    }
	    case cOIType : ClassOrInterfaceType => {
	      cOIType.getName
	    }
	    case _ => {
	      throw new Exception( "class type not yet handled" )
	    }
	  };

	if ( (ftypName.length > 4)
	    && (ftypName.substring( 0, 4 ) == "List") ) {
	  val cascadeType : ArrayInitializerExpr =
	    new ArrayInitializerExpr(
	      new java.util.LinkedList[Expression]()
	    );
	      cascadeType.getValues().add( 
		new FieldAccessExpr(
		  new NameExpr( "CascadeType" ),
		  "ALL"
		)
	      );
	      cPairs.add(
		new MemberValuePair(
		  "cascade",
		  cascadeType
		)
	      );
	      cPairs.add(
		new MemberValuePair(
		  "fetch",
		  new FieldAccessExpr(
		    new NameExpr( "FetchType" ),
		    "LAZY"
		  )
		)
	      );
	      cPairs.add(
		new MemberValuePair(
		  "mappedBy",
		  new StringLiteralExpr(
		    (trgtResourceClassName.substring( 0, 1 ).toLowerCase
		     + trgtResourceClassName.substring(
		       1,
		       trgtResourceClassName.length ))
		  )
		)
	      );
	      // cPairs.add(
// 		new MemberValuePair(
// 		  "targetEntity",
// 		  new NameExpr(
// 		    ftypName.substring( 4, ftypName.length ) + ".class"
// 		  )
// 		)
// 	      );
	      columnAnnotation.setName(
		ASTHelper.createNameExpr( "OneToMany" )
	      );
	    }
	else {
	      cPairs.add(
		new MemberValuePair(
		  "name",
		  new StringLiteralExpr( nameStem )
		)
	      );
	      cPairs.add(
		new MemberValuePair(
		  "unique",
		  new BooleanLiteralExpr( false )
		)
	      );
	      cPairs.add(
		new MemberValuePair(
		  "nullable",
		  new BooleanLiteralExpr( true )
		)
	      );
	      cPairs.add(
		new MemberValuePair(
		  "insertable",
		  new BooleanLiteralExpr( true )
		)
	      );
	      cPairs.add(
		new MemberValuePair(
		  "updatable",
		  new BooleanLiteralExpr( true )
		)
	      );
	      columnAnnotation.setName(
		ASTHelper.createNameExpr( "Column" )
	      );

	  if (nameStem == trgtIdFldName) {
	    annotations.add(
	      new MarkerAnnotationExpr(
		ASTHelper.createNameExpr( "Id" )
		)
	    )
	    // annotations.add(
// 	      new MarkerAnnotationExpr(
// 		ASTHelper.createNameExpr( "GeneratedValue" )
// 	      )
// 	    )
	  }
	}
	
	annotations.add( columnAnnotation );
	methodDecl.setAnnotations( annotations );
	
	methodDecl
      }

    def camelBackAccessor( propName : String, accessor : String ) : String = {
      (accessor
       + propName.substring( 0, 1 ).toUpperCase
       + propName.substring( 1, propName.length )
       )
    }

    def addAccessorMethod(
      propType : String,
      propName : String,
      isId : Boolean,
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration
    ) : CompilationUnit = {
      val getMethod : MethodDeclaration =
	new MethodDeclaration(
	  ModifierSet.PUBLIC,
	  new ClassOrInterfaceType( propType ),
	  camelBackAccessor( propName, "get" )
	);
      
      val getBlock : BlockStmt = new BlockStmt();
      val fieldAccessExpr : FieldAccessExpr =
	new FieldAccessExpr(
	  new ThisExpr(),
	  trgtUniqueIdFldName
	);
      val retStmt : ReturnStmt =
	new ReturnStmt( fieldAccessExpr );      
      
      val annotations = new java.util.LinkedList[AnnotationExpr]();
      if (isId) {
	annotations.add( 
	  new MarkerAnnotationExpr(
 	    ASTHelper.createNameExpr( "Id" )
 	  )
	);
      }
      val cPairs = new java.util.LinkedList[MemberValuePair]();
      cPairs.add(
	new MemberValuePair(
	  "name",
	  new StringLiteralExpr( propName )
	)
      );
      cPairs.add(
	new MemberValuePair(
	  "unique",
	  new BooleanLiteralExpr( true )
	)
      );
      cPairs.add(
	new MemberValuePair(
	  "nullable",
	  new BooleanLiteralExpr( false )
	)
      );
      cPairs.add(
	new MemberValuePair(
	  "insertable",
	  new BooleanLiteralExpr( true )
	)
      );
      cPairs.add(
	new MemberValuePair(
	  "updatable",
	  new BooleanLiteralExpr( true )
	)
      );

      val columnAnnotation : NormalAnnotationExpr =
	new NormalAnnotationExpr(
	  ASTHelper.createNameExpr( "Column" ),
	  cPairs
	);

      annotations.add( columnAnnotation );
      getMethod.setAnnotations( annotations );
      ASTHelper.addMember( typ, getMethod );      
      getMethod.setBody( getBlock );
      ASTHelper.addStmt( getBlock, retStmt );      

      val setMethod : MethodDeclaration =
	new MethodDeclaration(
	  ModifierSet.PUBLIC,
	  ASTHelper.VOID_TYPE,
	  camelBackAccessor( propName, "set" )
	);
      val paramModel : Parameter =
	ASTHelper.createParameter(
	  new ClassOrInterfaceType( propType ),
	  "id"
	);
      
      val setBlock : BlockStmt = new BlockStmt();
      val assignExpr : AssignExpr =
	new AssignExpr(
	  new FieldAccessExpr( new ThisExpr(), trgtUniqueIdFldName ),
	  new NameExpr( "id" ),
	  AssignExpr.Operator.assign
	  );                 
      
      ASTHelper.addMember( typ, setMethod );      
      setMethod.setParameters( new java.util.LinkedList[Parameter]() );
      setMethod.getParameters.add( paramModel );
      setMethod.setBody( setBlock );
      ASTHelper.addStmt( setBlock, assignExpr );      

      cUnit
    }

    def addSQLIdAccessors(
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration
    ) : CompilationUnit = {
      //addAccessorMethod( "String", trgtUniqueIdFldName, false, cUnit, typ );
      //addAccessorMethod( "String", trgtIdFldName, true, cUnit, typ );
      
      cUnit
    }

    def addSQLGetters(
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration
    ) : CompilationUnit = {
      // Nothing to do for now
      cUnit
    }
    def addSQLSetters(
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration
    )
    : CompilationUnit = {
      // Nothing to do for now
      cUnit
    }
    def addSQLRemovers(
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration
    )
    : CompilationUnit = {
      // Nothing to do for now
      cUnit
    }

    def getContainedFields : Seq[FieldDeclaration] = {
      for (member <-
	   scala.collection.jcl.Conversions.convertList(
	     srcCompilationUnit.getTypes.get(0).getMembers
	   ) if (member.isInstanceOf[FieldDeclaration]
		 //&& (ModifierSet.isPublic( member.getMembers ))
		 //&& (ModifierSet.isFinal( member.getMembers ))
	       ))
      yield member.asInstanceOf[FieldDeclaration]
    }

    def addSQLResourceModelContentMembers(
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration
      ) : CompilationUnit = {

	def addFieldAccessMethods(
	  acc : ClassOrInterfaceDeclaration,
	  member : FieldDeclaration,
	  v : VariableDeclarator
	) = {
	  val nameStem : String = v.getId.getName;
	  val accessMethod : MethodDeclaration =
	    new MethodDeclaration(
	      ModifierSet.PUBLIC,
	      trgtRenderType( cUnit, typ, member ), 
	      camelBackAccessor( nameStem, "get" )
	    );
	  val accessBlock : BlockStmt = new BlockStmt();
	  val accessCallModel : FieldAccessExpr =	      
	    new FieldAccessExpr(
	      new ThisExpr(),
	      nameStem
	    );
	  
	  addColumnAnnotations(
	    accessMethod,
	    member,
	    nameStem,
	    new java.util.LinkedList[AnnotationExpr]()
	  );
	  
	  accessMethod.setBody( accessBlock );
	  ASTHelper.addStmt(
	    accessBlock,
	    new ReturnStmt( accessCallModel )
	  );
	  
	  if (!ModifierSet.isFinal( member.getModifiers )) {
	    val updateMethod : MethodDeclaration =
	      new MethodDeclaration(
		ModifierSet.PUBLIC,
		ASTHelper.VOID_TYPE, 
		camelBackAccessor( nameStem, "set" )
	      );
	    val updateParam : Parameter =
	      ASTHelper.createParameter(
		trgtRenderType( cUnit, typ, member ),
		nameStem
	      );
	    val updateBlock : BlockStmt = new BlockStmt();
	    val updateCallModel : FieldAccessExpr =	      
	      new FieldAccessExpr( new ThisExpr(), nameStem );
	    val updateExpr : AssignExpr =
	      new AssignExpr(
		updateCallModel,
		new NameExpr( nameStem ),
		AssignExpr.Operator.assign
	      );	    
	    updateMethod.setBody( updateBlock );
	    ASTHelper.addStmt(
	      updateBlock,
	      updateExpr
	    );
	    updateMethod.setParameters(
	      new java.util.LinkedList[Parameter]()
	    );
	    updateMethod.getParameters.add( updateParam );
	    
	    ASTHelper.addMember( typ, updateMethod );
	  }
	  
	  ASTHelper.addMember( typ, accessMethod );	    	    
	}

      val containedFields : Seq[FieldDeclaration]
	= getContainedFields;

	// fold annotatedMethods into resource class
	( typ /: containedFields )({
	  ( acc : ClassOrInterfaceDeclaration,
	    member : FieldDeclaration )
	  => {
	    scala.collection.jcl.Conversions.convertList(
	      member.getVariables
	      ).map( { 
	      ( v : VariableDeclarator ) => {
		addFieldAccessMethods( acc, member, v )
	      }
	    } );
	    typ
	  }
       });
	cUnit
    }
    def createSQLizedModelResource() : Option[CompilationUnit] = {
      val ( cUnit, typ ) =
	addSQLResourceClass(
	  trgtImportDecls(
	    addSQLPackage(
	      trgtCompilationUnit
	    )
	  )
	);

      if ((resourceIsConcrete) && !(isCollectionType( typ ))) {
	addSQLResourceModelContentMembers(
	  addSQLRemovers(
	    addSQLSetters(
	      addSQLGetters(
		addSQLIdAccessors(
		  addSQLResourceCtor(
		    addSQLResourceCtxtFields( cUnit, typ ),
		    typ ), typ), typ ), typ ), typ ), typ)
      }
	
      Some( cUnit )
    }
  }
  def transformSources(
    projectName : String,
    dbName : String,
    dirLocation : String,
    fileLocations : List[String]
  )
  : List[Option[CompilationUnit]] = {
    for ( loc <- fileLocations )
    yield SourceTransformer( projectName, dirLocation + loc, dbName ).createSQLizedModelResource()
  }
  def transformSourcesDir(
    projectName : String,
    dbName : String,
    location : String
  ) 
  : List[Option[CompilationUnit]] = {
    transformSources(
      projectName,
      dbName,
      location,
      (for ( fileName <- new java.io.File( location ).list if
	fileName.contains( ".java" ) )
	yield fileName
      ).toList
    )
  }
  def trgtResourceClassName( cUnit : CompilationUnit ) : String = {
      cUnit.getTypes.get(0).getName
    }
  def generateSQLizedResourceClassFiles(
    projectName : String,
    dbName : String,
    srcLocation : String,
    trgtLocation : String
  ) 
  : Unit = {
    val trgtFile = new java.io.File( trgtLocation );
    if (!( trgtFile ).exists) {
      trgtFile.mkdirs()
    }
    for ( mcUnit <- transformSourcesDir( projectName, dbName, srcLocation ) )
      yield {
	mcUnit match {
	  case Some( cUnit ) => {
	    try {
	      val fileName : String =
		trgtLocation + trgtResourceClassName( cUnit ) + ".java";
              val out : java.io.BufferedWriter =
		new java.io.BufferedWriter(
		  new java.io.FileWriter(
		    fileName
		  )
		);
	      
	      System.out.println( "<writing>" + fileName + "</writing>\n" );
              out.write( cUnit.toString );
	      out.flush();
              out.close();
	    } catch
	    {
	      case e => e.printStackTrace( System.err )
	    }
	  }
	  case None => {
	    System.out.println(
	      ("<skipping>" + "abstract class" + "</skipping>\n")
	    );
	  }
	}
      }
  }
  def generateSQLizedResources(
    projectName : String,
    dbName : String,
    srcLocation : String
    ) : Unit = {
      println( "<SQLGeneration>" );
      val slashEndedSrcLoc = {
	val l = srcLocation.length;
	if ( srcLocation.substring( l-1, l ) == "/" )
	  srcLocation
	else (srcLocation + "/");
      }

      println( "<sourceDirectory>" + slashEndedSrcLoc + "</sourceDirectory>" );
      println( "<projectName>" + projectName + "</projectName>" );
      println( "<storeName>" + dbName + "</storeName>" );

      generateSQLizedResourceClassFiles(
	projectName,
	dbName,
	slashEndedSrcLoc,
	slashEndedSrcLoc + "persistence/sql/"
	)
      println( "</SQLGeneration>" );
    }
}

object theDesdemona extends Desdemona {
  def makeST(
    projectName : String,
    location : String,
    store : String
  ) : SourceTransformer = {
    new SourceTransformer( projectName, location, store )
  }
  def printUsage = {
    println( "desdemona <configOpt>" );
    println( " where <configOpt> ::= " );
    println( "     --groupId <groupId>" );
    println( "     --projectName <projName>" );
    println( "     --storeName <storeName>" );
    println( "     --srcLocation <srcLocation>" );
  }
  
  def argMap( args: Array[String] )
  : Option[scala.collection.mutable.HashMap[String,String]] = {
    if ( (args.length > 0) && (args.length % 2 == 0) ) {
      val argMap =
	new scala.collection.mutable.HashMap[String,String]();      
      val range = (0 to ((args.length / 2) - 1) );
      for ( idx <- range.force ) yield {
	val kv = Tuple( args( idx*2 ), args( idx*2 + 1 ) );
	argMap + kv
      };
      Some( argMap )
    }
    else None
  }

  def main(args: Array[String]) {    
    argMap( args ) match {
      case Some( map ) => {
	println("Desdemona initiated with " + map);
	try {
	  map.get( "--projectName" ) match {
	    case Some( projName ) => {
	      map.get( "--groupId" ) match {
		case Some( groupId ) => {
		  map.get( "--storeName" ) match {
		    case Some( storeName ) => {
		      map.get( "--srcLocation" ) match {
			case Some( srcLocation ) => {
			  generateSQLizedResources(
			    projName,
			    storeName,
			    (srcLocation
			     + "/"
			     + groupId.replace( ".", "/" )
			     + "/"
			     + "model"
			     + "/"
			     + projName
			     + "/"
			     + "Absyn")
			    )
			}
			case None =>
			  throw new Exception( "srcLocation not specified" )
		      }
		    }
		    case None =>
		      throw new Exception( "store name not specified" )
		  }
		}
		case None =>
		  throw new Exception( "groupId not specified" )
	      }
	    }
	    case None =>
	      throw new Exception( "project name not specified" )
	  }
	}
	catch {
	  case e => {
	    //println( e.getMessage )
	    e.printStackTrace
	    printUsage
	  }
	}
      }
      case None => printUsage
    }
  }
}

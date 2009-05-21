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

import java.io.FileInputStream

trait Ophelia {
  case class SourceTransformer(
    location : String,
    store : String
  ) {
    val _trgtPackageDependencies : List[String] =
      List(
	"com.biosimilarity.reflection.model",
	"com.sun.jersey.api.NotFoundException",
	"javax.ws.rs.DELETE",
	"javax.ws.rs.GET",
	"javax.ws.rs.PUT",
	"javax.ws.rs.Produces",
	"javax.ws.rs.QueryParam",
	"javax.ws.rs.Path",
	"javax.ws.rs.PathParam",
	"javax.ws.rs.core.Context",
	"javax.ws.rs.core.Request",
	"javax.ws.rs.core.Response",
	"javax.ws.rs.core.UriInfo",
	"javax.persistence.CascadeType",
	"javax.persistence.Column",
	"javax.persistence.Entity",
	"javax.persistence.FetchType",
	"javax.persistence.Id",
	"javax.persistence.OneToMany",
	"javax.persistence.Table",
	"javax.persistence.UniqueConstraint",	
	"java.util.Date",
	"java.util.HashSet",
	"java.util.Set",
	"java.util.Iterator",
	"java.net.URI"	
	)
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
    def trgtPackageSuffix : String = ".resources"
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
	//new ClassOrInterfaceType( "String" )
	member.getType // BUGBUG lgm should make copy
      }
    def trgtRenderMethodName : String = "render"
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
	 (_trgtPackageDependencies ::: List( srcPackageName + ".*", trgtPackageName + ".*" )))(
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
    def addSQLResourceClass( cUnit : CompilationUnit )
    : ( CompilationUnit, ClassOrInterfaceDeclaration ) = {
      val typ : ClassOrInterfaceDeclaration =
	new ClassOrInterfaceDeclaration(
	  ModifierSet.PUBLIC,
	  false,
	  trgtResourceClassName
	);
      ASTHelper.addTypeDeclaration( cUnit, typ );
      typ.setAnnotations( new java.util.LinkedList[AnnotationExpr]() );
      typ.getAnnotations.add(
	new MarkerAnnotationExpr(
	  ASTHelper.createNameExpr( "Entity" )
	)
      );
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
      typ.setExtends( new java.util.LinkedList[ClassOrInterfaceType]() );
      typ.getExtends.add( trgtRootClass );      
      
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
      ASTHelper.addMember(typ, uuidField);
      cUnit
    }
    def addSQLResourceCtor(
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration
    )
    : CompilationUnit = {
      val ctor : ConstructorDeclaration =
	new ConstructorDeclaration( 
	  ModifierSet.PUBLIC,
	  trgtResourceClassName
	);

      val block : BlockStmt = new BlockStmt();
      val callModel : MethodCallExpr =	      
	new MethodCallExpr(
	  new ThisExpr(),
	  "super",
	  new java.util.LinkedList[Expression]()
	);      
      ASTHelper.addStmt( block, callModel );      
      
      ctor.setParameters( new java.util.LinkedList[Parameter]() )
      for ( mparam <- resourceModelMinimalCtorParams )
	yield {
	  ctor.getParameters.add( mparam );
	  callModel.getArgs.add(
	    new NameExpr( mparam.getId.toString )
	  )
	};

      ASTHelper.addMember( typ, ctor );
      ctor.setBlock( block );
      cUnit
    }

    def addSQLIdAccessors(
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration
    ) : CompilationUnit = {
      val getMethod : MethodDeclaration =
	new MethodDeclaration(
	  ModifierSet.PUBLIC,
	  new ClassOrInterfaceType( "String" ),
	  "uuid"
	);
      
      val getBlock : BlockStmt = new BlockStmt();
      val fieldAccessExpr : FieldAccessExpr =
	new FieldAccessExpr(
	  new ThisExpr(),
	  trgtUniqueIdFldName
	);
      val retStmt : ReturnStmt =
	new ReturnStmt( fieldAccessExpr );      

      val setMethod : MethodDeclaration =
	new MethodDeclaration(
	  ModifierSet.PUBLIC,
	  ASTHelper.VOID_TYPE,
	  "uuid"
	);
      val paramModel : Parameter =
	ASTHelper.createParameter(
	  new ClassOrInterfaceType( "String" ),
	  "id"
	);
      
      val setBlock : BlockStmt = new BlockStmt();
      val assignExpr : AssignExpr =
	new AssignExpr(
	  new FieldAccessExpr( new ThisExpr(), trgtUniqueIdFldName ),
	  new NameExpr( "id" ),
	  AssignExpr.Operator.assign
	  );                 

      ASTHelper.addMember( typ, getMethod );      
      getMethod.setBody( getBlock );
      ASTHelper.addStmt( getBlock, retStmt );

      ASTHelper.addMember( typ, setMethod );      
      setMethod.setParameters( new java.util.LinkedList[Parameter]() );
      setMethod.getParameters.add( paramModel );
      setMethod.setBody( setBlock );
      ASTHelper.addStmt( setBlock, assignExpr );

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
    def addSQLResourceModelContentMembers(
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration
      ) : CompilationUnit = {
      val containedFields : Seq[FieldDeclaration]
      =
	(for (member <-
	     scala.collection.jcl.Conversions.convertList(
	       srcCompilationUnit.getTypes.get(0).getMembers
	       ) if (member.isInstanceOf[FieldDeclaration]
		     //&& (ModifierSet.isPublic( member.getMembers ))
		     //&& (ModifierSet.isFinal( member.getMembers ))
		   ))
	  yield member.asInstanceOf[FieldDeclaration]);

	// fold annotatedMethods into resource class
	( typ /: containedFields )({
	  ( acc : ClassOrInterfaceDeclaration,
	    member : FieldDeclaration )
	  =>
	  {
	    val nameStem : String =
	      (scala.collection.jcl.Conversions.convertList(
		member.getVariables
	      ))( 0 ).getId.getName;
	    val accessMethod : MethodDeclaration =
	      new MethodDeclaration(
		ModifierSet.PUBLIC,
		trgtRenderType( cUnit, typ, member ), 
		("get"
		 + nameStem.substring( 0, 1 ).toUpperCase
		 + nameStem.substring( 1, nameStem.length )
		 )
	      );
	    val accessBlock : BlockStmt = new BlockStmt();
	    val accessCallModel : MethodCallExpr =	      
	      new MethodCallExpr(
		new ThisExpr(),
		nameStem
	      );
	    val updateMethod : MethodDeclaration =
	      new MethodDeclaration(
		ModifierSet.PUBLIC,
		trgtRenderType( cUnit, typ, member ), 
		("set"
		 + nameStem.substring( 0, 1 ).toUpperCase
		 + nameStem.substring( 1, nameStem.length )
		 )
	      );
	    val updateBlock : BlockStmt = new BlockStmt();
	    val updateCallModel : MethodCallExpr =	      
	      new MethodCallExpr(
		new ThisExpr(),
		nameStem
	      );
	    val updateExpr : AssignExpr =
	      new AssignExpr(
		updateCallModel,
		new NameExpr( nameStem ),
		AssignExpr.Operator.assign
		);	    

	    accessMethod.setAnnotations(
	      new java.util.LinkedList[AnnotationExpr]()
	    );
	    val columnAnnotation : NormalAnnotationExpr =
	      new NormalAnnotationExpr(
		ASTHelper.createNameExpr( "Column" ),
		new java.util.LinkedList[MemberValuePair]()
	      );
	    accessMethod.getAnnotations.add(
	      columnAnnotation
	    );
	    columnAnnotation.getPairs().add(
	      new MemberValuePair(
		"name",
		new StringLiteralExpr( nameStem )
	      )
	    );
	    columnAnnotation.getPairs().add(
	      new MemberValuePair(
		"unique",
		new BooleanLiteralExpr( false )
	      )
	    );
	    columnAnnotation.getPairs().add(
	      new MemberValuePair(
		"nullable",
		new BooleanLiteralExpr( true )
	      )
	    );
	    columnAnnotation.getPairs().add(
	      new MemberValuePair(
		"insertable",
		new BooleanLiteralExpr( true )
	      )
	    );
	    columnAnnotation.getPairs().add(
	      new MemberValuePair(
		"updatable",
		new BooleanLiteralExpr( true )
	      )
	    );

	    accessMethod.setBody( accessBlock );
	    ASTHelper.addStmt(
	      accessBlock,
	      new ReturnStmt( accessCallModel )
	    );

	    updateMethod.setBody( updateBlock );
	    ASTHelper.addStmt(
	      updateBlock,
	      updateExpr
	    );

	    ASTHelper.addMember( typ, accessMethod );
	    ASTHelper.addMember( typ, updateMethod );
	    
	    typ
	  }
       });
	cUnit
    }
    def createSQLizedModelResource() : CompilationUnit = {
      val ( cUnit, typ ) =
	addSQLResourceClass(
	  trgtImportDecls(
	    addSQLPackage(
	      trgtCompilationUnit
	    )
	  )
	);
      addSQLResourceModelContentMembers(
	addSQLRemovers(
	  addSQLSetters(
	    addSQLGetters(
	      addSQLIdAccessors(
	      addSQLResourceCtor(
		addSQLResourceCtxtFields( cUnit, typ ),
		typ ), typ), typ ), typ ), typ ), typ);

      cUnit
    }
  }
  def transformSources(
    dbName : String,
    dirLocation : String,
    fileLocations : List[String]
  )
  : List[CompilationUnit] = {
    for ( loc <- fileLocations )
    yield SourceTransformer( dirLocation + loc, dbName ).createSQLizedModelResource()
  }
  def transformSourcesDir( dbName : String, location : String ) 
  : List[CompilationUnit] = {
    transformSources(
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
    dbName : String,
    srcLocation : String,
    trgtLocation : String
  ) 
  : Unit = {
    val trgtFile = new java.io.File( trgtLocation );
    if (!( trgtFile ).exists) {
      trgtFile.mkdirs()
    }
    for ( cUnit <- transformSourcesDir( dbName, srcLocation ) )
      yield {
	try {
	  val fileName : String = trgtLocation + trgtResourceClassName( cUnit ) + ".java";
	  //println( "acquiring writer for " + fileName );
          val out : java.io.BufferedWriter =
	    new java.io.BufferedWriter(
	      new java.io.FileWriter(
		fileName
	      )
	    );
	  //println( "acquired writer for " + fileName );
	  //println( "writing " + fileName + " with " + cUnit.toString);
	  System.out.println( "writing " + fileName + "\n" );
          out.write( cUnit.toString );
	  //println( "wrote " + fileName );
	  out.flush();
	  //println( "flushing " + fileName );
          out.close();
	  //println( "closing " + fileName );
	} catch
	{
	  case e => e.printStackTrace( System.err )
	}
      }
  }
  def generateSQLizedResources(
    dbName : String,
    srcLocation : String
    ) : Unit = {
      generateSQLizedResourceClassFiles(
	dbName,
	srcLocation,
	srcLocation + "resources/"
	)
    }
}

object theOphelia extends Ophelia {
}

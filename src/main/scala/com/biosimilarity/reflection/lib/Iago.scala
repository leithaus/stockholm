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

trait Iago {
  case class SourceTransformer( location : String ) {
    val _trgtPackageDependencies : List[String] =
      List(
	"com.sap.dspace.model.othello.BaseRESTfulBehavior",	
	"com.sap.dspace.model.Model",
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
    def trgtRootClassName : String = "BaseRESTfulBehavior"
    def trgtRootClass : ClassOrInterfaceType = {
      new ClassOrInterfaceType( trgtRootClassName )
    }
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
      member : MethodDeclaration
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
    def addRESTPackage( cUnit : CompilationUnit )
    : CompilationUnit = {
      cUnit.setPackage(
	new PackageDeclaration(
	  ASTHelper.createNameExpr( srcPackageName + trgtPackageSuffix )
	)
      );
      cUnit
    }
    def addRESTResourceClass( cUnit : CompilationUnit )
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
	new SingleMemberAnnotationExpr(
	  ASTHelper.createNameExpr( "Path" ),
	  new StringLiteralExpr( "/" + resourceModelName )
	)
      );
      typ.getAnnotations.add(
	new SingleMemberAnnotationExpr(
	  ASTHelper.createNameExpr( "Produces" ),
	  new StringLiteralExpr( "application/xml" )
	)
      );
      typ.setExtends(
	new java.util.LinkedList[ClassOrInterfaceType]()
      );
      typ.getExtends.add( trgtRootClass )
      
      (	cUnit, typ )
    }
    def addRESTResourceCtxtFields(
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration
    )
    : CompilationUnit = {
      val uriInfoField : FieldDeclaration =
	ASTHelper.createFieldDeclaration(
	  ModifierSet.PUBLIC,
	  new ClassOrInterfaceType( "UriInfo" ),
	  "uriInfo"
	);
      val requestField : FieldDeclaration =
	ASTHelper.createFieldDeclaration(
	  ModifierSet.PUBLIC,
	  new ClassOrInterfaceType( "Request" ),
	  "request"
	);
      val containerField : FieldDeclaration =
	ASTHelper.createFieldDeclaration(
	  ModifierSet.PUBLIC,
	  ASTHelper.createReferenceType("String", 0),
	  "container"
	);
      val modelMemoField : FieldDeclaration =
	ASTHelper.createFieldDeclaration(
	  ModifierSet.PRIVATE,
	  new ClassOrInterfaceType( resourceModelName ),
	  modelMemoFieldName
	);
      uriInfoField.setAnnotations( new java.util.LinkedList[AnnotationExpr]() );
      uriInfoField.getAnnotations.add(
	new MarkerAnnotationExpr( ASTHelper.createNameExpr( "Context" ) )
      );
      requestField.setAnnotations( new java.util.LinkedList[AnnotationExpr]() );
      requestField.getAnnotations.add(
	new MarkerAnnotationExpr( ASTHelper.createNameExpr( "Context" ) )
      );
      ASTHelper.addMember(typ, uriInfoField);
      ASTHelper.addMember(typ, requestField);
      ASTHelper.addMember(typ, containerField);
      ASTHelper.addMember(typ, modelMemoField);
      cUnit
    }
    def addRESTResourceCtor(
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration
    )
    : CompilationUnit = {
      val ctor : ConstructorDeclaration =
	new ConstructorDeclaration( 
	  ModifierSet.PUBLIC,
	  trgtResourceClassName
	);
      val paramUriInfo : Parameter =
	ASTHelper.createParameter(
	  new ClassOrInterfaceType( "UriInfo" ),
	  "uriInfo"
	);
      val paramRequest : Parameter =
	ASTHelper.createParameter(
	  new ClassOrInterfaceType( "Request" ),
	  "request"
	);
      val paramContainer : Parameter =
	ASTHelper.createParameter(
	  ASTHelper.createReferenceType("String", 0),
	  "container"
	);
      val block : BlockStmt = new BlockStmt();
      val callModel : MethodCallExpr =	      
	new MethodCallExpr(
	  new ThisExpr(),
	  "model"
	);      
      val callModelCtor : ObjectCreationExpr =	      
	new ObjectCreationExpr(
	  null,
	  new ClassOrInterfaceType( resourceModelName ),
	  new java.util.LinkedList[Expression]()
	);      

      ctor.setParameters( new java.util.LinkedList[Parameter]() )
      for ( mparam <- resourceModelMinimalCtorParams )
	yield {
	  ctor.getParameters.add( mparam );
	  callModelCtor.getArgs.add(
	    new NameExpr( mparam.getId.toString )
	  )
	};
      ctor.getParameters.add( paramUriInfo );
      ctor.getParameters.add( paramRequest );
      ctor.getParameters.add( paramContainer );

      ASTHelper.addMember( typ, ctor );
      ctor.setBlock( block );
      ASTHelper.addStmt( block, callModel );
      ASTHelper.addArgument( callModel, callModelCtor );
      cUnit
    }

    def addRESTModelAccessors(
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration
    ) : CompilationUnit = {
      val getMethod : MethodDeclaration =
	new MethodDeclaration(
	  ModifierSet.PUBLIC,
	  new ClassOrInterfaceType( resourceModelDecl.getName ),
	  "model"
	);
      
      val getBlock : BlockStmt = new BlockStmt();
      val fieldAccessExpr : FieldAccessExpr =
	new FieldAccessExpr(
	  new ThisExpr(),
	  modelMemoFieldName
	);
      val retStmt : ReturnStmt =
	new ReturnStmt( fieldAccessExpr );      

      val setMethod : MethodDeclaration =
	new MethodDeclaration(
	  ModifierSet.PUBLIC,
	  ASTHelper.VOID_TYPE,
	  "model"
	);
      val paramModel : Parameter =
	ASTHelper.createParameter(
	  new ClassOrInterfaceType( resourceModelDecl.getName ),
	  "m"
	);
      
      val setBlock : BlockStmt = new BlockStmt();
      val assignExpr : AssignExpr =
	new AssignExpr(
	  new FieldAccessExpr( new ThisExpr(), modelMemoFieldName ),
	  new NameExpr( "m" ),
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

    def restGetterName : String = {
      "get" + resourceModelDecl.getName
    }
    def restSetterName : String = {
      "put" + resourceModelDecl.getName
    }
    def restRemoverName : String = {
      "delete" + resourceModelDecl.getName
    }
    def addRESTGetters(
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration
    ) : CompilationUnit = {
      val method : MethodDeclaration =
	new MethodDeclaration(
	  ModifierSet.PUBLIC,
	  new ClassOrInterfaceType( resourceModelDecl.getName ),
	  restGetterName
	);
      val param : Parameter =
	ASTHelper.createParameter(
	  ASTHelper.createReferenceType("String", 0),
	  "search"
	);
      val block : BlockStmt = new BlockStmt();
      val annote : MarkerAnnotationExpr =
	new MarkerAnnotationExpr( ASTHelper.createNameExpr( "GET" ) );
      val qAnnote : SingleMemberAnnotationExpr =
	new SingleMemberAnnotationExpr(
	  ASTHelper.createNameExpr( "QueryParam" ),
	  new StringLiteralExpr( "search" )
	);
      val retStmt : ReturnStmt =
	new ReturnStmt(
	  new NullLiteralExpr()
	  );

      param.setAnnotations( new java.util.LinkedList[AnnotationExpr]() );
      param.getAnnotations.add( qAnnote );
      
      ASTHelper.addParameter(method, param);
      ASTHelper.addMember(typ, method);      
      method.setAnnotations( new java.util.LinkedList[AnnotationExpr]() );
      method.getAnnotations.add( annote );
      method.setBody(block);
      ASTHelper.addStmt( block, retStmt );

      cUnit
    }
    def addRESTSetters(
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration
    )
    : CompilationUnit = {
      val method : MethodDeclaration =
	new MethodDeclaration(
	  ModifierSet.PUBLIC,
	  new ClassOrInterfaceType( "Response" ),
	  restSetterName
	);
      val block : BlockStmt = new BlockStmt();
      val annote : MarkerAnnotationExpr =
	new MarkerAnnotationExpr( ASTHelper.createNameExpr( "PUT" ) );
      val callHibernateModel : MethodCallExpr =	      
	new MethodCallExpr(
	  new NameExpr( "Model" ),
	  "merge"
	);      
      val callModel : MethodCallExpr =	      
	new MethodCallExpr(
	  new ThisExpr(),
	  "model"
	);
      val retStmt : ReturnStmt =
	new ReturnStmt(
	  new MethodCallExpr(
	    {
	      val callCreated : MethodCallExpr =
		new MethodCallExpr(
		  new NameExpr( "Response" ),
		  "created"
		);
	      ASTHelper.addArgument(
		callCreated,
		new MethodCallExpr(
		  new NameExpr( "uriInfo" ),
		  "getAbsolutePath"
		  )
	      );
	      callCreated
	     },
	    "build"
	    )
	);      

      ASTHelper.addMember(typ, method);      
      method.setAnnotations( new java.util.LinkedList[AnnotationExpr]() );
      method.getAnnotations.add( annote );
      method.setBody(block);
      ASTHelper.addStmt( block, callHibernateModel );
      ASTHelper.addArgument( callHibernateModel, callModel );
      ASTHelper.addStmt( block, retStmt );

      cUnit
    }
    def addRESTRemovers(
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration
    )
    : CompilationUnit = {
      val method : MethodDeclaration =
	new MethodDeclaration(
	  ModifierSet.PUBLIC,
	  ASTHelper.VOID_TYPE,
	  restRemoverName
	);
      val annote : MarkerAnnotationExpr =
	new MarkerAnnotationExpr( ASTHelper.createNameExpr( "DELETE" ) );
      val block : BlockStmt = new BlockStmt();

      ASTHelper.addMember(typ, method);
      method.setAnnotations( new java.util.LinkedList[AnnotationExpr]() );
      method.getAnnotations.add( annote );
      method.setBody(block);

      cUnit
    }
    def addRESTResourceModelContentMembers(
      cUnit : CompilationUnit,
      typ : ClassOrInterfaceDeclaration
      ) : CompilationUnit = {
      val annotatedMethods : Seq[MethodDeclaration]
      =
	(for (member <-
	     scala.collection.jcl.Conversions.convertList(
	       srcCompilationUnit.getTypes.get(0).getMembers
	       ) if (member.isInstanceOf[MethodDeclaration] &&
	       (member.getAnnotations != null)))
	  yield member.asInstanceOf[MethodDeclaration]);

	// fold annotatedMethods into resource class
	( typ /: annotatedMethods )({
	  ( acc : ClassOrInterfaceDeclaration,
	    member : MethodDeclaration )
	  =>
	  {
	    val accessMethod : MethodDeclaration =
	      new MethodDeclaration(
		ModifierSet.PUBLIC,
		trgtRenderType( cUnit, typ, member ), 
		member.getName
	      );
	    val block : BlockStmt = new BlockStmt();
	    val callModel : MethodCallExpr =	      
	      new MethodCallExpr(
		new MethodCallExpr(
		  new ThisExpr(),
		  "model"
		  ),
		member.getName
	      );

	    var pathStr : String =
	      member.getName.replace( "get", "/" );
	    if ( !( pathStr.contains( "/" ) ) )
	      pathStr = "/" + pathStr;

	    ASTHelper.addMember( typ, accessMethod );

	    accessMethod.setAnnotations( new java.util.LinkedList[AnnotationExpr]() );
	    accessMethod.getAnnotations.add(
	      new SingleMemberAnnotationExpr(
		ASTHelper.createNameExpr( "Path" ),
		new StringLiteralExpr( pathStr )
	      )
	    );
	    accessMethod.getAnnotations.add(
	      new MarkerAnnotationExpr( new NameExpr( "GET" ) )
	    );

	    accessMethod.setBody(block);
	    ASTHelper.addStmt( block, new ReturnStmt( callModel ) );
	    
	    typ
	  }
       });
	cUnit
    }
    def createRESTfulModelResource() : CompilationUnit = {
      val ( cUnit, typ ) =
	addRESTResourceClass(
	  trgtImportDecls(
	    addRESTPackage(
	      trgtCompilationUnit
	    )
	  )
	);
      addRESTResourceModelContentMembers(
	addRESTRemovers(
	  addRESTSetters(
	    addRESTGetters(
	      addRESTModelAccessors(
	      addRESTResourceCtor(
		addRESTResourceCtxtFields( cUnit, typ ),
		typ ), typ), typ ), typ ), typ ), typ);

      cUnit
    }
  }
  def transformSources( dirLocation : String, fileLocations : List[String] )
  : List[CompilationUnit] = {
    for ( loc <- fileLocations )
    yield SourceTransformer( dirLocation + loc ).createRESTfulModelResource()
  }
  def transformSourcesDir( location : String ) 
  : List[CompilationUnit] = {
    transformSources(
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
  def generateRESTfulResourceClassFiles(
    srcLocation : String,
    trgtLocation : String
  ) 
  : Unit = {
    val trgtFile = new java.io.File( trgtLocation );
    if (!( trgtFile ).exists) {
      trgtFile.mkdirs()
    }
    for ( cUnit <- transformSourcesDir( srcLocation ) )
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
  def generateRESTfulResources(
    srcLocation : String
    ) : Unit = {
      generateRESTfulResourceClassFiles(
	srcLocation,
	srcLocation + "resources/"
	)
    }
}

object theIago extends Iago {
}

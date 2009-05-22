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
	"java.util.Date",
	"java.util.HashSet",
	"java.util.Set",
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
	 (trgtPackageDependencies ::: List( srcPackageName + ".*", trgtPackageName + ".*" )))(
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
      def getFirstId( fd : FieldDeclaration ) : VariableDeclaratorId = {
	(scala.collection.jcl.Conversions.convertList(
	  fd.getVariables
	  ))( 0 ).getId
      }

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
      for (
	fieldDecl <- getContainedFields;
	mparam = new Parameter(
	  fieldDecl.getType,
	  getFirstId( fieldDecl )
	)
      )
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
	      new StringLiteralExpr( trgtResourceClassName )
	    )
	  );
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
	}
	
	annotations.add( columnAnnotation );
	methodDecl.setAnnotations( annotations );
	
	methodDecl
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
      
      val annotations = new java.util.LinkedList[AnnotationExpr]();
      annotations.add( 
	new MarkerAnnotationExpr(
 	  ASTHelper.createNameExpr( "Id" )
 	)
      );
      val cPairs = new java.util.LinkedList[MemberValuePair]();
      cPairs.add(
	new MemberValuePair(
	  "name",
	  new StringLiteralExpr( trgtUniqueIdFldName )
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
      val containedFields : Seq[FieldDeclaration]
	= getContainedFields;

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
	    val accessCallModel : FieldAccessExpr =	      
	      new FieldAccessExpr(
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
	    val updateCallModel : FieldAccessExpr =	      
	      new FieldAccessExpr( new ThisExpr(), nameStem );
	    val updateExpr : AssignExpr =
	      new AssignExpr(
		updateCallModel,
		new NameExpr( nameStem ),
		AssignExpr.Operator.assign
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
    def createSQLizedModelResource() : Option[CompilationUnit] = {
      if (resourceIsConcrete) {
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
	
	Some( cUnit )
      }
      else None
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
	      val fileName : String = trgtLocation + trgtResourceClassName( cUnit ) + ".java";
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
	    System.out.println( "<skipping>abstract class" + "</skipping>\n" );
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

object theOphelia extends Ophelia {
  def printUsage = {
    println( "ophelia <configOpt>" );
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
	println("Ophelia initiated with " + map);
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
	    println( e.getMessage )
	    printUsage
	  }
	}
      }
      case None => printUsage
    }
  }
}

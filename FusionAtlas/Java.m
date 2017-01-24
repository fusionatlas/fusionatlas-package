(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



BeginPackage["FusionAtlas`Java`",{"FusionAtlas`","JLink`","FusionAtlas`Bigraphs`","FusionAtlas`GraphPairs`","FusionAtlas`TensorSolver`","FusionAtlas`DisplayGraphs`"}];


FusionAtlasJavaDirectory::usage="";FusionAtlasScalaDirectory::usage="";


AsScalaSet;AsScalaList;AsScalaOption;AsScalaObject;FromScalaObject;UnpackJavaLists;UnpackJavaObjects;
GraphToScalaObject;


RestartJava;CleanScalaLibraries;BuildScalaLibraries;RebuildScalaLibraries;


Begin["`Private`"];


FusionAtlasJavaDirectory[]:=ToFileName[FusionAtlasDirectory[],"java"]


FusionAtlasScalaDirectory[]:=ToFileName[FusionAtlasDirectory[],"scala"]


(* Add old java libraries to the classpath *)
AddToClassPath[ToFileName[FusionAtlasJavaDirectory[],"target"],ToFileName[FusionAtlasJavaDirectory[],"jars"]];

(* Next, check that everything we need exists. If not, run sbt to build things. *)
ScalaVersion=StringCases[Import[ToFileName[{FusionAtlasScalaDirectory[]},"build.sbt"]],"scalaVersion := \""~~v:(Except["\""]...)~~"\"":>v][[1]];
ScalaMajorVersion=StringTake[ScalaVersion,4];
ProjectJars:=FileNames[{ToFileName[{FusionAtlasScalaDirectory[],"target","scala-"<>ScalaMajorVersion},"*.jar"]}];
CleanScalaLibraries[]:=Module[{},
Quiet[
DeleteDirectory[ToFileName[{FusionAtlasScalaDirectory[],"lib_managed"}],DeleteContents->True];
DeleteDirectory[ToFileName[{FusionAtlasScalaDirectory[],"target"}],DeleteContents->True];,
DeleteDirectory::nodir
];
]
BuildScalaLibraries[]:=Module[{},
SetDirectory[FusionAtlasScalaDirectory[]];
If[$OperatingSystem==="Windows",
Run["sbt.bat clean update package"];
Run["./sbt clean update package"];
];
ResetDirectory[];
AddToClassPath[ProjectJars];
]
RebuildScalaLibraries[]:=(CleanScalaLibraries[];BuildScalaLibraries[])
If[Length[ProjectJars]==0,
Print["The Scala libraries for the FusionAtlas haven't been compiled. I'm doing that now (Scala version "<>ScalaMajorVersion<>")..."];
Print["Go and get a coffee, as this takes a few minutes, or longer if you have a slow connection."];
CleanScalaLibraries[];
RebuildScalaLibraries[];
If[Length[ProjectJars]==0,
Print["Compilation failed. Please complain to Scott Morrison <scott@tqft.net>."];
]
];

AddToClassPath[FileNames[ToFileName[{FusionAtlasScalaDirectory[],"target","scala-"<>ScalaMajorVersion,"classes"},"log4j.xml"]]];
AddToClassPath[FileNames[ToFileName[{$HomeDirectory,".sbt","boot","scala-"<>ScalaVersion,"lib"},"*.jar"]]];
AddToClassPath[FileNames[ToFileName[{FusionAtlasScalaDirectory[],"lib_managed","jars"},"*/*/*.jar"]]];
AddToClassPath[FileNames[ToFileName[{FusionAtlasScalaDirectory[],"lib_managed","bundles"},"*/*/*.jar"]]];
AddToClassPath[ProjectJars];


RestartJava[]:=Module[{},
If[!NumericQ[Global`$JVMHeap],Global`$JVMHeap=512];
javaPath=FileNameJoin[{RunThrough["echo \\\"$(/usr/libexec/java_home -v 1.7)\\\"",""],"bin","java"}];
ReinstallJava[CommandLine->javaPath,JVMArguments->"-Xmx"<>ToString[Global`$JVMHeap]<>"m"];
AllowRaggedArrays[True];
LoadJavaClass["org.fusionatlas.graphs.PairOfBigraphsWithDuals$","AllowShortContext"->False];
LoadJavaClass["org.fusionatlas.graphs.Bigraph$","AllowShortContext"->False];
LoadJavaClass["org.fusionatlas.graphs.BigraphWithDuals$","AllowShortContext"->False];
LoadJavaClass["org.fusionatlas.graphs.FusionGraph$","AllowShortContext"->False];
LoadJavaClass["org.fusionatlas.graphs.DirectedFusionGraph$","AllowShortContext"->False];
LoadJavaClass["scala.collection.immutable.List$","AllowShortContext"->False];
LoadJavaClass["scala.collection.immutable.Set$","AllowShortContext"->False];
LoadJavaClass["scala.Some$","AllowShortContext"->False];
LoadJavaClass["scala.None$","AllowShortContext"->False];
LoadJavaClass["java.util.Arrays","AllowShortContext"->False];
LoadJavaClass["scala.collection.JavaConversions","AllowShortContext"->False];
LoadJavaClass["net.tqft.toolkit.algebra.fusion.FusionRing$"];
LoadJavaClass["net.tqft.toolkit.algebra.matrices.Matrix$"];
LoadJavaClass["org.fusionatlas.graphs.obstructions.TriplePointObstruction$"];
]


RestartJava[]


AsScalaSet[list_]:=scala`collection`JavaConversions`asScalaSet[JavaNew["java.util.HashSet",java`util`Arrays`asList[MakeJavaObject[list]]]]@toSet[]
AsScalaSet[{}]:=scala`collection`immutable`Set$`MODULE$@empty[]


AsScalaOption[{}]:=scala`None$`MODULE$
AsScalaOption[{x_}]:=scala`Some$`MODULE$@apply[MakeJavaObject[x]]


unpackListRules={Y_/;InstanceOf[Y,"scala.collection.Traversable"]:>JavaConversions`seqAsJavaList[Y@toList[]],Y_/;InstanceOf[Y,"scala.Tuple2"]->Y@productIterator[]@toList[],Y_/;InstanceOf[Y,"scala.Tuple3"]->Y@productIterator[]@toList[],Y_/;InstanceOf[Y,"scala.Tuple4"]->Y@productIterator[]@toList[],Y_/;InstanceOf[Y,"java.util.List"]:>Y@toArray[],Y_/;InstanceOf[Y,"net.tqft.toolkit.algebra.matrices.Matrix"]:>Y@entries[]};


UnpackJavaLists[X_]:=X//.unpackListRules


FromScalaObject[X_]:=X//.unpackListRules~Join~{Y_/;InstanceOf[Y,"net.tqft.toolkit.mathematica.MathematicaExpression"]:>ToExpression[Y@toMathematicaInputString[]]}~Join~{Y_/;InstanceOf[Y,"net.tqft.toolkit.algebra.fusion.FusionRing"]:>FusionRules[_,{{0,0,0}->UnpackJavaLists[Y@structureCoefficients[]]}]}


AsScalaList[list:{__Integer|__Real}]:=scala`collection`JavaConversions`asScalaBuffer[java`util`Arrays`asList[MakeJavaObject/@list]]@toList[]
AsScalaList[list:{__List}]:=AsScalaList[AsScalaList/@list]
AsScalaList[list_List]:=scala`collection`JavaConversions`asScalaBuffer[java`util`Arrays`asList[MakeJavaObject[list]]]@toList[]
AsScalaList[{}]:=scala`collection`immutable`List$`MODULE$@empty[]


AsScalaObject[x_List]:=AsScalaList[x]
AsScalaObject[FusionRules[_,{{0,0,0}->m_}]]:=net`tqft`toolkit`algebra`fusion`FusionRing$`MODULE$@apply[AsScalaList[net`tqft`toolkit`algebra`matrices`Matrix$`MODULE$@from[Map[AsScalaList,#,{0,1}]]&/@m]]
AsScalaObject[g_GradedBigraph]:=org`fusionatlas`graphs`Bigraph$`MODULE$@apply[GraphToString[g]]
AsScalaObject[g_BigraphWithDuals]:=org`fusionatlas`graphs`BigraphWithDuals$`MODULE$@apply[GraphToString[g]]
AsScalaObject[{g1_BigraphWithDuals,g2_BigraphWithDuals}]:=org`fusionatlas`graphs`PairOfBigraphsWithDuals$`MODULE$@apply[GraphToString[g1],GraphToString[g2]]
AsScalaObject[X_/;JavaObjectQ[X]]:=X


UnpackJavaObjects[_]:=(Print["The UnpackJavaObjects function has been deprecated; please use FromScalaObject instead."];Abort[]);
GraphToScalaObject[_]:=(Print["The GraphToScalaObject function has been deprecated; please use AsScalaObject instead."];Abort[]);


End[];


EndPackage[];

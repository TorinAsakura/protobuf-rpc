name := "plugin"

version := "1.0"

scalaVersion := "2.11.11"

import sbtprotobuf.{ProtobufPlugin=>PB}

Seq(PB.protobufSettings: _*)

version in protobufConfig := "2.6.1"

javaSource in PB.protobufConfig <<= (baseDirectory in Compile)(_ / "src/generated/java")

protocOptions in PB.protobufConfig += "--proto_path=" + sourceDirectory.value + "/main/proto"

test in assembly := {}

// as3

//assemblyJarName in assembly := "protobuf-rpc-as3.jar"

//mainClass in assembly := Some("ru.rknrl.rpc.MainAs3")

// js

// assemblyJarName in assembly := "protobuf-rpc-js.jar"

// mainClass in assembly := Some("ru.rknrl.rpc.MainJs")

// ts

// assemblyJarName in assembly := "protobuf-rpc-ts.jar"

// mainClass in assembly := Some("ru.rknrl.rpc.MainTs")

// scala

assemblyJarName in assembly := "protobuf-rpc-scala.jar"

mainClass in assembly := Some("ru.rknrl.rpc.MainScala")

// java

//assemblyJarName in assembly := "protobuf-rpc-java.jar"

//mainClass in assembly := Some("ru.rknrl.rpc.MainJava")
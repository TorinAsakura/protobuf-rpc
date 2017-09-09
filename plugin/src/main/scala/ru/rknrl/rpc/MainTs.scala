//       ___       ___       ___       ___       ___
//      /\  \     /\__\     /\__\     /\  \     /\__\
//     /::\  \   /:/ _/_   /:| _|_   /::\  \   /:/  /
//    /::\:\__\ /::-"\__\ /::|/\__\ /::\:\__\ /:/__/
//    \;:::/  / \;:;-",-" \/|::/  / \;:::/  / \:\  \
//     |:\/__/   |:|  |     |:/  /   |:\/__/   \:\__\
//      \|__|     \|__|     \/__/     \|__|     \/__/

package ru.rknrl.rpc

import java.io.{PrintWriter, StringWriter}

import com.google.protobuf.DescriptorProtos.DescriptorProto
import com.google.protobuf.ExtensionRegistry
import com.google.protobuf.compiler.PluginProtos.{CodeGeneratorRequest, CodeGeneratorResponse}
import ru.rknrl.rpc.Utils._

import scala.collection.JavaConversions.asScalaBuffer

object MainTs {
  def main(args: Array[String]) {
    val extensionRegistry = ExtensionRegistry.newInstance()

    Options.registerAllExtensions(extensionRegistry)

    val request = CodeGeneratorRequest.parseFrom(System.in, extensionRegistry)
    createResponse(request).writeTo(System.out)
    System.out.flush()
  }

  def createResponse(request: CodeGeneratorRequest) =
    try {
      CodeGeneratorResponse.newBuilder.addFile(createFiles(request)).build
    } catch {
      case t: Throwable ⇒
        val sw = new StringWriter
        t.printStackTrace(new PrintWriter(sw))
        CodeGeneratorResponse.newBuilder.setError(sw.toString).build
    }

  def createFiles(request: CodeGeneratorRequest) = {
    val messageNameToPackage = Utils.messageNameToPackage(request).toMap

    def serverFile(messages: Seq[DescriptorProto], `package`: String) =
      CodeGeneratorResponse.File.newBuilder
        .setName("server.ts")
        .setContent(serverContent(messages, `package`))
        .build

    def serverContent(messages: Seq[DescriptorProto], `package`: String) =
      s"""/* GENERATED. DO NOT MODIFY */
         |import { EventEmitter } from 'events'
         |import * as protobuf from 'protobufjs'
         |import { protos } from './protos'
         |
         |interface IConnection {
         |  send(messageId: number, messageWriter: protobuf.Writer): void;
         |  on(eventName: string, listener: (...args: any[]) => void): this;
         |}
         |
         |class Server extends EventEmitter {
         |  constructor(public connection: IConnection) {
         |    super()
         |
         |    connection.on('message', this.onMessage)
         |  }
         |
         |  onMessage = (messageId: number, messageBytes: Uint8Array) => {
         |    switch (messageId) {
         |      ${serverReceiveContent(messages)}
         |    }
         |  }
         |
         |  ${serverSendContent(messages)}
         |}
         |
         |export default Server
    """.stripMargin


    def serverReceiveContent(messages: Seq[DescriptorProto]) =
      messages.map(receiveContent).mkString

    def receiveContent(d: DescriptorProto) =
      s"""
         |      case ${msgId(d)}:
         |        this.emit('${d.getName}', protos.${d.getName}.decodeDelimited(messageBytes));
         |        break;
      """.stripMargin

    def serverSendContent(messages: Seq[DescriptorProto]) =
      messages.map(sendContent).mkString

    def sendContent(d: DescriptorProto) =
      s"""
         |  ${methodName(d)}(${sendMethodParameters(d).map(x ⇒ x + ": any").mkString(", ")}) {
         |    this.connection.send(
         |      ${msgId(d)},
         |      protos.${className(d)}.encodeDelimited(
         |        protos.${className(d)}.create({
         |          ${sendMethodParameters(d).map(x ⇒ x + ": " + x).mkString(", ")}
         |        })
         |      )
         |    );
         |  }
    """.stripMargin


    def sendMethodParameters(d: DescriptorProto) =
      d.getFieldList.map(f ⇒ fieldName(f))

    val messages = request.getProtoFileList
      .filter(f ⇒ request.getFileToGenerateList.contains(f.getName))
      .flatMap(f ⇒ f.getMessageTypeList
        .filter(m ⇒ m.hasOptions && m.getOptions.hasExtension(Options.msgid))
      )

    serverFile(messages, "")
  }

  def methodName(d: DescriptorProto) = uncapitalize(d.getName)

  def sendMethodName(d: DescriptorProto) = "send" + d.getName

}

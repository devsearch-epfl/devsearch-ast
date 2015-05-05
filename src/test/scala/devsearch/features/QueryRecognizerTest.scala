package devsearch.features

import devsearch.ast.ContentsSource
import devsearch.parsers.{GoParser, Languages}
import org.scalatest.FlatSpec

class QueryRecognizerTest extends FlatSpec {
  "Query recognizer" should "recognize JavaScript code" in {
    val jsSource = """function FCKTextColorCommand_OnClick( ev, command, color )
                     |{
                     |    this.className = 'ColorDeselected' ;
                     |    command.SetColor( color ) ;
                     |    command._Panel.Hide() ;
                     |}""".stripMargin

    assert(QueryRecognizer.findCodeFile(jsSource) match {
      case Some(codeFile) => codeFile.language == Languages.JavaScript
      case None => false
    })
  }

  it should "recognize Java files" in {
    val javaSource = """boolean loaded = false;
                       |URL systemIndependentPath = getClass().getResource(path);
                       |try
                       |{
                       |    // If the URL was unsuccessfully made make a last attempt to create a URL.
                       |    if(systemIndependentPath == null)
                       |        this.bufferedImage = ImageIO.read(new URL("file:" + path));
                       |    else
                       |        this.bufferedImage = ImageIO.read(systemIndependentPath);
                       |    loaded = true;
                       |}
                       |catch(Exception ex)
                       |{
                       |    this.bufferedImage = null;
                       |    System.out.println("Could not load image: " + path);
                       |}""".stripMargin

    assert(QueryRecognizer.findCodeFile(javaSource) match {
      case Some(codeFile) => codeFile.language == Languages.Java
      case None => false
    })
  }

  it should "recognize Go files" in {
    val goSource = """// Copyright 2014-2015 The project AUTHORS. All rights reserved.
                     |// Use of this source code is governed by a BSD-style
                     |// license that can be found in the LICENSE file.
                     |
                     |package main
                     |
                     |import (
                     |  "fmt"
                     |)
                     |
                     |func test() {
                     |  t := true
                     |  nums := []int{2, 3, 4}
                     |  sum := 0
                     |  for _, num := range nums {
                     |    sum += num
                     |  }
                     |  fmt.Println("sum:",sum)
                     |}""".stripMargin

    assert(QueryRecognizer.findCodeFile(goSource) match {
      case Some(codeFile) => codeFile.language == Languages.Go
      case None => false
    })
  }

  it should "recognize Scala files" in {
    val scalaSource = """def map(f: Node => Node)(implicit ops: GraphOps {
                        |  type Node = self.Node
                        |  type Edge = self.Edge
                        |}): ops.Graph = {
                        |  val newNodes = nodes.map(f)
                        |  val nodeMapping = (nodes zip newNodes).toMap
                        |  val newEdges = edges.map(e => ops.newEdge(e, nodeMapping(e.from), nodeMapping(e.to)))
                        |  ops.newGraph(newNodes, newEdges)
                        |}""".stripMargin

    assert(QueryRecognizer.findCodeFile(scalaSource) match {
      case Some(codeFile) => codeFile.language == Languages.Scala
      case None => false
    })
  }
}

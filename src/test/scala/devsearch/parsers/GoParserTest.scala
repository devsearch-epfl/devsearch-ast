package devsearch.parsers

import org.scalatest._
import devsearch.ast._
import Modifiers._
import Empty._

class GoParserTest extends FlatSpec with ParserTest {

  lazy val barGo = new ContentsSource("bar.go", """
    // Copyright 2014-2015 The DevMine authors. All rights reserved.
    // Use of this source code is governed by a BSD-style
    // license that can be found in the LICENSE file.
    package main

    import "fmt"

    const (
      maxIter = 200
    )

    // myFunc is a very cool func!
    func myFunc(y int) int {
      x := y + 100

      for i := 0; i < maxIter; i++ {
        tmp := 0
        for j := 0; j < maxIter; j++ {
          tmp += j
        }

        x += tmp
      }

      fmt.Println(x)

      return 42
    }

    func main() {
      bar := myFunc(10)

      fmt.Println(bar)
    }
   """)

  lazy val rangeGo = new ContentsSource("range.go", """
    // Copyright 2014-2015 The project AUTHORS. All rights reserved.
    // Use of this source code is governed by a BSD-style
    // license that can be found in the LICENSE file.

    package main

    import (
      "fmt"
    )

    func test() {
      t := true
      nums := []int{2, 3, 4}
      sum := 0
      for _, num := range nums {
        sum += num
      }
      fmt.Println("sum:",sum)
    }
  """)

  lazy val fooGo = new ContentsSource("foo.go", """
    // Copyright 2014-2015 The DevMine authors. All rights reserved.
    // Use of this source code is governed by a BSD-style
    // license that can be found in the LICENSE file.

    package main

    import (
      "fmt"
    )

    var (
      x = 42
    )

    const (
      plop = "plop"
    )

    // myFunc is the greatest function ever written
    func myFunc(foo, bar string, y int) {
      flop := 42.20
      if flop > 42 {
        fmt.Println("YAHOO")
      }
      fmt.Println(foo, bar, x, plop, y, flop)

      for i := 0; i < 5; i++ {
        fmt.Println(i)
      }
    }

    func main() {
      myFunc("foo", "bar", 7)
    }
  """)

  "Go parser" should "not leave empty positions in bar.go" in {
    checkPositions(GoParser.parse(barGo))
  }

  it should "not leave empty positions in range.go" in {
    checkPositions(GoParser.parse(rangeGo))
  }

  it should "not leave empty positions in foo.go" in {
    checkPositions(GoParser.parse(fooGo))
  }

  it should "work for parsing bar.go" in {
    val ast = GoParser.parse(barGo)
    val name = ast match {
      case p : PackageDef => p.name
      case _ => Names.DEFAULT
    }

    assert(ast == PackageDef(name,List(),
      List(Import("fmt",false,false)), List(
        ValDef(PROTECTED | FINAL,"maxIter",List(),NoType,SimpleLiteral(PrimitiveTypes.Int,"200"),false),
        FunctionDef(NoModifiers,"myFunc",List(),List(),List(),NoType,Block(List(
          Assign(Ident("x"),BinaryOp(Ident("y"), "+", SimpleLiteral(PrimitiveTypes.Int, "100")),None),
          For(List(),List(),NoExpr,List(),Block(List(
            Assign(Ident("tmp"),SimpleLiteral(PrimitiveTypes.Int, "0"),None),
            For(List(),List(),NoExpr,List(),Block(List(
              Assign(Ident("tmp"),Ident("j"),None))
            )),
            Assign(Ident("x"),Ident("tmp"),None)
          ))),
          FunctionCall(FieldAccess(Ident("fmt"),"Println",List()),List(),List(Ident("x"))),
          Return(SimpleLiteral(PrimitiveTypes.Int,"42"))))
        ),
        FunctionDef(NoModifiers,"main",List(),List(),List(),NoType,Block(List(
          Assign(Ident("bar"),FunctionCall(FieldAccess(NoExpr,"myFunc",List()),List(),List(SimpleLiteral(PrimitiveTypes.Int, "10"))),None),
          FunctionCall(FieldAccess(Ident("fmt"),"Println",List()),List(),List(Ident("bar")))
        )))
      )))
  }

  it should "work for parsing ranges" in {
    val ast = GoParser.parse(rangeGo)
    val name = ast match {
      case p : PackageDef => p.name
      case _ => Names.DEFAULT
    }

    assert(ast == PackageDef(name,List(),
      List(Import("fmt",false,false)),List(
        FunctionDef(NoModifiers,"test",List(),List(),List(),NoType,Block(List(
          Assign(Ident("t"),SimpleLiteral(PrimitiveTypes.Boolean,"true"),None),
          Assign(Ident("nums"),NoExpr,None), // should actually be an array, but they aren't parsed by the Go parser currently
          Assign(Ident("sum"),SimpleLiteral(PrimitiveTypes.Int,"0"),None),
          Foreach(List(
            ValDef(NoModifiers,"_",List(),NoType,NoExpr,false),
            ValDef(NoModifiers,"num",List(),NoType,NoExpr,false)
          ),Ident("nums"),Block(List(
            Assign(Ident("sum"),Ident("num"),None)
          ))),
          FunctionCall(FieldAccess(Ident("fmt"),"Println",List()),List(),List(SimpleLiteral(PrimitiveTypes.String,"sum:"), Ident("sum")))))))))
  }

  it should "work for parsing foo.go" in {
    val ast = GoParser.parse(fooGo)
    val name = ast match {
      case p : PackageDef => p.name
      case _ => Names.DEFAULT
    }

    assert(ast == PackageDef(name,List(),List(Import("fmt",false,false)),List(
      ValDef(PROTECTED | FINAL,"plop",List(),NoType,SimpleLiteral(PrimitiveTypes.String,"plop"),false),
      ValDef(PROTECTED,"x",List(),NoType,SimpleLiteral(PrimitiveTypes.Int,"42"),false),
      FunctionDef(NoModifiers,"myFunc",List(),List(),List(),NoType,Block(List(
        Assign(Ident("flop"),SimpleLiteral(PrimitiveTypes.Float,"42.20"),None),
        If(BinaryOp(Ident("flop"),">",SimpleLiteral(PrimitiveTypes.Int,"42")),Block(List(
          FunctionCall(FieldAccess(Ident("fmt"),"Println",List()),List(),List(SimpleLiteral(PrimitiveTypes.String,"YAHOO")))
        )),Block(List())),
        FunctionCall(FieldAccess(Ident("fmt"),"Println",List()),List(),List(Ident("foo"), Ident("bar"), Ident("x"), Ident("plop"), Ident("y"), Ident("flop"))),
        For(List(),List(),NoExpr,List(),Block(List(
          FunctionCall(FieldAccess(Ident("fmt"),"Println",List()),List(),List(Ident("i"))))))
      ))),
      FunctionDef(NoModifiers,"main",List(),List(),List(),NoType,Block(List(
        FunctionCall(FieldAccess(NoExpr,"myFunc",List()),List(),List(
          SimpleLiteral(PrimitiveTypes.String,"foo"),
          SimpleLiteral(PrimitiveTypes.String,"bar"),
          SimpleLiteral(PrimitiveTypes.Int,"7")
        ))
      ))))))
  }

  it should "be robust for queries" in {
    val source = new ContentsSource("file.go", """
      |import "fmt"
      |
      |func main() {
      |    fmt.Println("Hello, 世界")
      |}""".stripMargin)

    GoParser.parse(source)
  }

  it should "be robust also with function code" in {
    val source = new ContentsSource("file.go", "fmt.Println(\"bob\")")
    GoParser.parse(source)
  }
}

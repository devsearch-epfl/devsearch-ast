package devsearch.parsers

import devsearch.ast._
import devsearch.ast.Modifiers._
import devsearch.ast.Empty._
import org.scalatest._

class JsParserTest extends FunSuite {

  def check(str: String, description: String): Unit = test(description) {
    JsParser.parse(new ContentsSource("NoFile", str))
  }

  check("var abc;", "Regular variable statement w/o assignment")
  check("var abc = 5;", "Regular variable statement with assignment")
  check("/* */;", "Multiline comment")
  check("/** **/;", "Double star multiline comment")
  check("var f = function(){;};", "Function expression in var assignment")
  check("hi; // moo\n;", "single line comment 1")
  check("var varwithfunction;", "Dont match keywords as substrings") // difference between `var withsomevar` and `"str"` (local search and lits)
  check("a + b;", "addition")
  check("'a';", "single string literal")
  check("'a\\n';", "single string literal with escaped return")
  check("\"a\";", "double string literal")
  check("\"a\\n\";", "double string literal with escaped return")
  check("\"var\";", "string is a keyword")
  check("\"variable\";", "string starts with a keyword")
  check("\"somevariable\";", "string contains a keyword")
  check("\"somevar\";", "string ends with a keyword")
  check("500;", "int literal")
  check("500.;", "float literal w/o decimals")
  check("500.432;", "float literal with decimals")
  check(".432432;", "float literal w/o int")
  check("(a,b,c);", "parens and comma")
  check("[1,2,abc];", "array literal")
  check("var o = {a:1};", "object literal unquoted key")
  check("var o = {\"b\":2};", "object literal quoted key") // opening curly may not be at the start of a statement...
  check("var o = {c:c};", "object literal keyname is identifier")
  check("var o = {a:1,\"b\":2,c:c};", "object literal combinations")
  check("var x;\nvar y;", "two lines")
  check("var x;\nfunction n(){; }", "function def")
  check("var x;\nfunction n(abc){; }", "function def with arg")
  check("var x;\nfunction n(abc, def){ ;}", "function def with args")
  check("function n(){ \"hello\"; }", "function def with body")
  check("/a/;", "regex literal")
  check("/a/b;", "regex literal with flag")
  check("/a/ / /b/;", "regex div regex")
  check("a/b/c;", "triple division looks like regex")

  // http://code.google.com/p/es-lab/source/browse/trunk/tests/parser/parsertests.js?r=86
  // http://code.google.com/p/es-lab/source/browse/trunk/tests/parser/parsertests.js?r=430

  // first tests for the lexer, should also parse as program (when you append a semi)

  // comments
  check("//foo!@#^&$1234\nbar;", "single line comment 2")
  check("/* abcd!@#@$* { } && null*/;", "single line multi line comment")
  check("/*foo\nbar*/;","multi line comment")
  check("/*x*x*/;","multi line comment with *")
  check("/**/;","empty comment")

  // identifiers
  check("x;","1 identifier")
  check("_x;","2 identifier")
  check("xyz;","3 identifier")
  check("$x;","4 identifier")
  check("x$;","5 identifier")
  check("_;","6 identifier")
  check("x5;","7 identifier")
  check("x_y;","8 identifier")
  check("x+5;","9 identifier")
  check("xyz123;","10 identifier")
  check("x1y1z1;","11 identifier")
  check("foo\\u00D8bar;","12 identifier unicode escape")
  //["fooï¿½bar;","13 identifier unicode embedded (might fail)")

  // numbers
  check("5;", "1 number")
  check("5.5;", "2 number")
  check("0;", "3 number")
  check("0.0;", "4 number")
  check("0.001;", "5 number")
  check("1.e2;", "6 number")
  check("1.e-2;", "7 number")
  check("1.E2;", "8 number")
  check("1.E-2;", "9 number")
  check(".5;", "10 number")
  check(".5e3;", "11 number")
  check(".5e-3;", "12 number")
  check("0.5e3;", "13 number")
  check("55;", "14 number")
  check("123;", "15 number")
  check("55.55;", "16 number")
  check("55.55e10;", "17 number")
  check("123.456;", "18 number")
  check("1+e;", "20 number")
  check("0x01;", "22 number")
  check("0XCAFE;", "23 number")
  check("0x12345678;", "24 number")
  check("0x1234ABCD;", "25 number")
  check("0x0001;", "26 number")

  // strings
  check("\"foo\";", "1 string")
  check("\"foo\";", "2 string")
  check("\"x\";", "3 string")
  check("\"\";", "4 string")
  check("\"foo\\tbar\";", "5 string")
  check("\"!@#$%^&*()_+{}[]\";", "6 string")
  check("\"/*test*/\";", "7 string")
  check("\"//test\";", "8 string")
  check("\"\\\\\";", "9 string")
  check("\"\\u0001\";", "10 string")
  check("\"\\uFEFF\";", "11 string")
  check("\"\\u10002\";", "12 string")
  check("\"\\x55\";", "13 string")
  check("\"\\x55a\";", "14 string")
  check("\"a\\\\nb\";", "15 string")
  check("\";\"", "16 string: semi in a string")
  check("\"a\\\\\\nb\";", "17 string: line terminator escape")
  check("\"é\";", "18 string: accent")

  // literals
  check("null;", "null")
  check("true;", "true")
  check("false;", "false")

  // regex
  check("/a/;", "1 regex")
  check("/abc/;", "2 regex")
  check("/abc[a-z]*def/g;", "3 regex")
  check("/\\b/;", "4 regex")
  check("/[a-zA-Z]/;", "5 regex")

  // program tests (for as far as they havent been covered above)

  // regexp
  check("/foo(.*)/g;", "another regexp")

  // arrays
  check("[];", "1 array")
  check("[   ];", "2 array")
  check("[1];", "3 array")
  check("[1,2];", "4 array")
  check("[1,2,,];", "5 array")
  check("[1,2,3];", "6 array")
  check("[1,2,3,,,];", "7 array")

  // objects
  check("{};", "1 object")
  check("({x:5});", "2 object")
  check("({x:5,y:6});", "3 object")
  check("({x:5,});", "4 object")
  check("({if:5});", "5 object")
  check("({ get x() {42;} });", "6 object")
  check("({ set y(a) {1;} });", "7 object")

  // member expression
  check("o.m;", "1 member expression")
  check("o[\"m\"];", "2 member expression")
  check("o[\"n\"][\"m\"];", "3 member expression")
  check("o.n.m;", "4 member expression")
  check("o.if;", "5 member expression")

  // call and invoke expressions
  check("f();", "1 call/invoke expression")
  check("f(x);", "2 call/invoke expression")
  check("f(x,y);", "3 call/invoke expression")
  check("o.m();", "4 call/invoke expression")
  check("o[\"m\"];", "5 call/invoke expression")
  check("o.m(x);", "6 call/invoke expression")
  check("o['m'](x);", "7 call/invoke expression")
  check("o.m(x,y);", "8 call/invoke expression")
  check("o['m'](x,y);", "9 call/invoke expression")
  check("f(x)(y);", "10 call/invoke expression")
  check("f().x;", "11 call/invoke expression")

  // eval
  check("eval('x');", "1 eval")
  check("(eval)('x');", "2 eval")
  check("(1,eval)('x');", "3 eval")
  check("eval(x,y);", "4 eval")

  // new expression
  check("new f();", "1 new expression")
  check("new o;", "2 new expression")
  check("new o.m;", "3 new expression")
  check("new o.m(x);", "4 new expression")
  check("new o.m(x,y);", "5 new expression")

  // prefix/postfix
  check("++x;", "1 pre/postfix")
  check("x++;", "2 pre/postfix")
  check("--x;", "3 pre/postfix")
  check("x--;", "4 pre/postfix")
  check("x ++;", "5 pre/postfix")
  check("x /* comment */ ++;", "6 pre/postfix")
  check("++ /* comment */ x;", "7 pre/postfix")

  // unary operators
  check("delete x;", "1 unary operator")
  check("void x;", "2 unary operator")
  check("+ x;", "3 unary operator")
  check("-x;", "4 unary operator")
  check("~x;", "5 unary operator")
  check("!x;", "6 unary operator")

  // meh
  check("new Date++;", "new date ++")
  check("+x++;", " + x ++")

  // expression expressions
  check("1 * 2;", "1 expression expressions")
  check("1 / 2;", "2 expression expressions")
  check("1 % 2;", "3 expression expressions")
  check("1 + 2;", "4 expression expressions")
  check("1 - 2;", "5 expression expressions")
  check("1 << 2;", "6 expression expressions")
  check("1 >>> 2;", "7 expression expressions")
  check("1 >> 2;", "8 expression expressions")
  check("1 * 2 + 3;", "9 expression expressions")
  check("(1+2)*3;", "10 expression expressions")
  check("1*(2+3);", "11 expression expressions")
  check("x<y;", "12 expression expressions")
  check("x>y;", "13 expression expressions")
  check("x<=y;", "14 expression expressions")
  check("x>=y;", "15 expression expressions")
  check("x instanceof y;", "16 expression expressions")
  check("x in y;", "17 expression expressions")
  check("x&y;", "18 expression expressions")
  check("x^y;", "19 expression expressions")
  check("x|y;", "20 expression expressions")
  check("x+y<z;", "21 expression expressions")
  check("x<y+z;", "22 expression expressions")
  check("x+y+z;", "23 expression expressions")
  check("x+y<z;", "24 expression expressions")
  check("x<y+z;", "25 expression expressions")
  check("x&y|z;", "26 expression expressions")
  check("x&&y;", "27 expression expressions")
  check("x||y;", "28 expression expressions")
  check("x&&y||z;", "29 expression expressions")
  check("x||y&&z;", "30 expression expressions")
  check("x<y?z:w;", "31 expression expressions")

  // assignment
  check("x >>>= y;", "1 assignment")
  check("x <<= y;", "2 assignment")
  check("x = y;", "3 assignment")
  check("x += y;", "4 assignment")
  check("x /= y;", "5 assignment")

  // comma
  check("x, y;", "comma")

  // block
  check("{};", "1 block")
  check("{x;};", "2 block")
  check("{x;y;};", "3 block")

  // vars
  check("var x;", "1 var")
  check("var x,y;", "2 var")
  check("var x=1,y=2;", "3 var")
  check("var x,y=2;", "4 var")

  // empty
  check(";", "1 empty")
  check("\n;", "2 empty")

  // expression statement
  check("x;", "1 expression statement")
  check("5;", "2 expression statement")
  check("1+2;", "3 expression statement")

  // if
  check("if (c) x; else y;", "1 if statement")
  check("if (c) x;", "2 if statement")
  check("if (c) {} else {};", "3 if statement")
  check("if (c1) if (c2) s1; else s2;", "4 if statement")

  // while
  check("do s; while (e);", "1 while statement")
  check("do { s; } while (e);", "2 while statement")
  check("while (e) s;", "3 while statement")
  check("while (e) { s; };", "4 while statement")

  // for
  check("for (;;) ;", "1 for statement")
  check("for (;c;x++) x;", "2 for statement")
  check("for (i;i<len;++i){};", "3 for statement")
  check("for (var i=0;i<len;++i) {};", "4 for statement")
  check("for (var i=0,j=0;;){};", "5 for statement")

  //["for (x in b; c; u) {};", "6 for statement")
  check("for ((x in b); c; u) {};", "7 for statement")
  check("for (x in a);", "8 for statement")
  check("for (var x in a){};", "9 for statement")
  check("for (var x=5 in a) {};", "10 for statement")
  check("for (var x = a in b in c) {};", "11 for statement")
  check("for (var x=function(){a+b;}; a<b; ++i) some;", "11 for statement, testing for parsingForHeader reset with the function")
  check("for (var x=function(){for (x=0; x<15; ++x) alert(foo); }; a<b; ++i) some;", "11 for statement, testing for parsingForHeader reset with the function 2")

  // flow statements
  check("continue;", "1 flow statement")
  check("continue label;", "2 flow statement")
  check("break;", "3 flow statement")
  check("break somewhere;", "4 flow statement")
  check("continue /* comment */ ;", "5 flow statement")
  check("continue \n;", "6 flow statement")
  check("return;", "7 flow statement")
  check("return 0;", "8 flow statement")
  check("return 0 + \n 1;", "9 flow statement")

  // with
  check("with (e) s;", "with statement")

  // switch
  check("switch (e) { case x: s; };", "1 switch statement")
  check("switch (e) { case x: s1;s2; default: s3; case y: s4; };", "2 switch statement")
  check("switch (e) { default: s1; case x: s2; case y: s3; };", "3 switch statement")
  check("switch (e) { default: s; };", "4 switch statement")
  check("switch (e) { case x: s1; case y: s2; };", "5 switch statement")

  // labels
  check("foo : x;", " flow statement")

  // throw
  check("throw x;", "1 throw statement")
  check("throw x\n;", "2 throw statement")

  // try catch finally
  check("try { s1; } catch (e) { s2; };", "1 trycatchfinally statement")
  check("try { s1; } finally { s2; };", "2 trycatchfinally statement")
  check("try { s1; } catch (e) { s2; } finally { s3; };", "3 trycatchfinally statement")

  // debugger
  check("debugger;", "debuger statement")

  // function decl
  check("function f(x) { e; return x; };", "1 function declaration")
  check("function f() { x; y; };", "2 function declaration")
  check("function f(x,y) { var z; return x; };", "3 function declaration")

  // function exp
  check("(function f(x) { return x; });", "1 function expression")
  check("(function empty() {;});", "2 function expression")
  check("(function empty() {;});", "3 function expression")
  check("(function (x) {; });", "4 function expression")

  // program
  check("var x; function f(){;}; null;", "1 program")
  check(";;", "2 program")
  check("{ x; y; z; }", "3 program")
  check("function f(){ function g(){;}};", "4 program")
  check("x;\n/*foo*/\n;", "5 program")

  // asi
  check("continue \n foo;", "1 asi")
  check("break \n foo;", "2 asi")
  check("return\nfoo;", "3 asi")
  check("var x; { 1 \n 2 } 3", "4 asi")
  check("ab   /* hi */\ncd", "5 asi")
  check("ab/*\n*/cd", "6 asi (multi line multilinecomment counts as eol)")
  check("continue /* wtf \n busta */ foo;", "7 asi illegal with multi line comment")
  check("function f() { s }", "8 asi")
  check("function f() { return }", "9 asi")

  // use strict
  check("\"use strict\"; \"bla\"\n; foo;", "1 directive")
  check("(function() { \"use strict\"; \"bla\";\n foo; });", "2 directive")
  check("\"use\\n strict\";", "3 directive")
  check("foo; \"use strict\";", "4 directive")

  // tests from http://es5conform.codeplex.com/

  check("\"use strict\"; var o = { eval: 42};", "8.7.2-3-1-s: the use of eval as property name is allowed")
  check("({foo:0,foo:1});", "Duplicate property name allowed in not strict mode")
  check("function foo(a,a){}", "Duplicate parameter name allowed in not strict mode")
  check("(function foo(eval){})", "Eval allowed as parameter name in non strict mode")
  check("(function foo(arguments){})", "Arguments allowed as parameter name in non strict mode")

  // empty programs
  check("", "1 Empty program")
  check("// test", "2 Empty program")
  check("//test\n", "3 Empty program")
  check("\n// test", "4 Empty program")
  check("\n// test\n", "5 Empty program")
  check("/* */", "6 Empty program")
  check("/*\ns,fd\n*/", "7 Empty program")
  check("/*\ns,fd\n*/\n", "8 Empty program")
  check("    ", "9 Empty program")
  check("  /*\nsmeh*/\n   ", "10 Empty program")

  // trailing whitespace
  check("a  ", "1 Trailing whitespace")
  check("a /* something */", "2 Trailing whitespace")
  check("a\n// hah", "3 Trailing whitespace")
  check("/abc/de//f", "4 Trailing whitespace")
  check("/abc/de/*f*/\n", "5 Trailing whitespace")

  // things the parser tripped over at one point or the other (prevents regression bugs)
  check("for (x;function(){ a\nb };z) x;", "for header with function body forcing ASI")
  check("c=function(){return;return};", "resetting noAsi after literal")
  check("d\nd()", "asi exception causing token overflow")
  check("for(;;){x=function(){}}", "function expression in a for header")
  check("for(var k;;){}", "parser failing due to ASI accepting the incorrect 'for' rule")
  check("({get foo(){ }})", "getter with empty function body")
  check("\nreturnr", "eol causes return statement to ignore local search requirement")
  check(" / /", "1 whitespace before regex causes regex to fail?")
  check("/ /// /", "2 whitespace before regex causes regex to fail?")
  check("/ // / /", "3 whitespace before regex causes regex to fail?")
  check("/ / / / /", "4 whitespace before regex causes regex to fail?")


  test("parsing is valid in weird fors") {
    val source = new ContentsSource("NoFile", "for (x;function(){ a\nb };z) x;")
    assert(JsParser.parse(source) == For(List(),List(Ident("x")),FunctionLiteral(List(),NoType,Block(List(Ident("a"), Ident("b")))),List(Ident("z")),Ident("x")))
  }

  test("parsing is valid in literal regex") {
    val source = new ContentsSource("NoFile", "/ /// /")
    assert(JsParser.parse(source) == FunctionCall(Ident("$$regexp"),List(),List(SimpleLiteral(PrimitiveTypes.String," "))))
  }
}

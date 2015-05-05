package devsearch.features

import devsearch.parsers.Languages
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
    val goSource = """func NewGridFont(texture *Texture, cellWidth, cellHeight int) *Font {
                     |    i := 0
                     |    glyphs := make(map[rune]*glyph)
                     |
                     |    for y := 0; y < int(texture.Height())/cellHeight; y++ {
                     |        for x := 0; x < int(texture.Width())/cellWidth; x++ {
                     |            g := &glyph{xadvance: float32(cellWidth)}
                     |            g.region = NewRegion(texture, x*cellWidth, y*cellHeight, cellWidth, cellHeight)
                     |            glyphs[rune(i)] = g
                     |            i += 1
                     |        }
                     |    }
                     |
                     |    return &Font{glyphs}
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

package devsearch.parsers

import org.scalatest.FlatSpec

class LanguagesTest extends FlatSpec {
  "Languages object" should "know about supported languages" in {
    assert(Languages.isLanguageSupported(Languages.Scala))
    assert(!Languages.isLanguageSupported("Fortran"))
  }

  it should "know whether a file is supported" in {
    assert(Languages.isFileSupported("simple.js"))
    assert(!Languages.isFileSupported("image.png"))
  }

  it should "find the correct parser" in {
    assert(Languages.parserFromFile("simple.js") == Some(JsParser))
    assert(Languages.parserFromFile("image.png") == None)
  }

  it should "guess the language from file name" in {
    assert(Languages.guess("simple.scala") == Some(Languages.Scala))
    assert(Languages.guess("image.png") == None)
  }

  it should "know the extensions for supported languages" in {
    assert(Languages.extension(Languages.Go) == Some("go"))
    assert(Languages.extension("Fortran") == None)
  }
}

import org.scalatest._

class HelloTest extends FlatSpec with Matchers {

  behavior of "Hello"

  it should "say Hello" in {
    Hello.say should equal ("Hello")
  }

  it should "not say Allô" in {
    Hello.say should not equal ("Allô")
  }

}

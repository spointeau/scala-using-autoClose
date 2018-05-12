import java.util.logging.Logger

object arm {
  type Resource = AutoCloseable

  case class ScopeResource(val x: Resource, val canThrow: Boolean = true, val doLog: Boolean = false)

  @annotation.implicitNotFound(msg = "Resource acquisition requires a scope.")
  final class Scope {
    var resources: List[ScopeResource] = Nil

    final def acquire(res: Resource, canThrow:Boolean = true, log:Boolean = false): Unit = {
      resources ::= ScopeResource(res,canThrow,log)
    }
  }

  object using {
    def apply[T](f: Scope => T): T = {

      var innerException : Option[Throwable] = Option.empty

      val scope = new Scope
      try
        return f(scope)
      catch {
        case e =>
          innerException = Some(e)
          throw e
      }
      finally {
        scope.resources.foreach { r =>
          try {
            r.x.close()
          }
          catch {
            case e: Throwable => {
              if (r.canThrow) {
                if (innerException.isEmpty) innerException = Some(e)
                else innerException.get.addSuppressed(e)
              }
              else {
                if( r.doLog ) Logger.getLogger("global").warning(e.getMessage)
              }
            }
          }
        }

        if (innerException.nonEmpty) throw innerException.get
      }
    }
  }

  def acquire[R <: Resource](res: R)(implicit in: Scope): R = {
    in.acquire(res)
    res
  }

  implicit class AutoCloseResource[T <: AutoCloseable](val x: T) extends AnyVal {
    def autoClose(canThrow:Boolean=true,log:Boolean=false)(implicit in: Scope): T = {
      in.acquire(x,canThrow,log)
      x
    }
  }
}

import arm._


class TestToClose(val name: String) extends AutoCloseable {

  override def close(): Unit = {
    println(s"Close $name")
    throw new Exception(s"Close Exception $name")
  }

  def getName() = name

}

object MainUsing extends App {

  System.setErr(System.out)

  try using { implicit scope =>
    val t = new TestToClose("test1").autoClose()
    val t2 = new TestToClose("test2").autoClose(canThrow = false, log = true)

    println(t.name)
  }
  catch {
    case e: Exception =>
      e.printStackTrace()
  }

}

package part2_abstract_math

import scala.util.{Failure, Success, Try}

object _4_Monads_Exercises extends App {

  import cats.Monad

  val monadList = Monad[List] // fetch the implicit monad for list
  val aList = monadList.pure(2)
  val extendedList = monadList.flatMap(aList)(x => List(x, x + 1))
  // Applicable to others like Option, Try, Future

  // either can be also a monad by wrapping it in a type of one argument
  val aManualEither: Either[String, Int] = Right(42)

  // this are monads for Either
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr]
  val errorMonad = Monad[ErrorOr]

  val anEither = loadingMonad.pure(0)
  val aChangedLoading = loadingMonad.flatMap(anEither)(n =>
    if (n < 42) Right(n) else Left("Loading meaning of life...")
  )

  // imaginary store
  case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(id: Long): LoadingOr[OrderStatus] =
    Right(OrderStatus(id, "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] = {
    if (orderStatus.orderId > 1000) Left("Not available")
    else Right("Montevideo, UY")
  }

  val orderId = 666L
  val orderLocation =
    loadingMonad.flatMap(getOrderStatus(orderId))(trackLocation)

  // use for-comprehension and extension methods
  var orderLocationFor = for {
    status <- getOrderStatus(orderId)
    location <- trackLocation(status)
  } yield location

  // TODO the service layer API of a web app
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4041"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def sendRequest(connection: Connection, payload: String): M[String]
  }

  /*
  Requirements:
    - return connection if cfg is correct, otherwise an error according to M type
    - return "request accepted" if payload is less then 30 chars, otherwise an error according to M type
   */

  private object HttpServiceOptionImpl extends HttpService[Option] {

    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)

    override def sendRequest(
        connection: Connection,
        payload: String
    ): Option[String] =
      if (payload.length < 30) Some("request accepted")
      else None
  }

  println("=== HttpServiceOptionImpl ===")
  println(for {
    cn <- HttpServiceOptionImpl.getConnection(config)
    resp <- HttpServiceOptionImpl.sendRequest(cn, "Hola, soy un payload")
  } yield resp)

  private object HttpServiceTryImpl extends HttpService[Try] {

    override def getConnection(cfg: Map[String, String]): Try[Connection] =
      (for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port))
        .map(Success(_))
        .getOrElse(Failure(new RuntimeException()))

    override def sendRequest(
        connection: Connection,
        payload: String
    ): Try[String] =
      if (payload.length < 30) Success("request accepted")
      else Failure(new RuntimeException("Invalid payload"))
  }

  println("=== HttpServiceTryImpl ===")

  println(for {
    cn <- HttpServiceTryImpl.getConnection(config)
    resp <- HttpServiceTryImpl.sendRequest(
      cn,
      "Hola, soy un payload9999999999999999999999999999"
    )
  } yield resp)

  private object HttpServiceErrorOrImpl extends HttpService[ErrorOr] {

    override def getConnection(
        cfg: Map[String, String]
    ): ErrorOr[Connection] = {
      if (!cfg.contains("host")) Left(new RuntimeException("missing host"))
      else if (!cfg.contains("port")) Left(new RuntimeException("missing port"))
      else Right(Connection(cfg("host"), cfg("port")))
    }

    override def sendRequest(
        connection: Connection,
        payload: String
    ): ErrorOr[String] =
      if (payload.length < 30) Right("request accepted")
      else Left(new RuntimeException("Invalid payload"))
  }

  println("=== HttpServiceErrorOrImpl ===")

  println(for {
    cn <- HttpServiceErrorOrImpl.getConnection(config)
    resp <- HttpServiceErrorOrImpl.sendRequest(
      cn,
      "Payloady McPayloadFace"
    )
  } yield resp)

  println(for {
    cn <- HttpServiceErrorOrImpl.getConnection(Map.empty)
    resp <- HttpServiceErrorOrImpl.sendRequest(
      cn,
      "Payloady McPayloadFace"
    )
  } yield resp)

  // GENERIC API

  import cats.syntax.flatMap._
  import cats.syntax.functor._
  def getResponse[M[_]: Monad](
      httpService: HttpService[M],
      config: Map[String, String],
      payload: String
  ): M[String] = for {
    con <- httpService.getConnection(config)
    res <- httpService.sendRequest(con, payload)
  } yield res

  getResponse(HttpServiceOptionImpl, config, "Hola")
  getResponse(HttpServiceTryImpl, config, "Hola")
  getResponse(HttpServiceErrorOrImpl, config, "Hola")

}

import scalaz.{-\/, Applicative, Coproduct, Free, FreeAp, Inject, Monad, NaturalTransformation, Nondeterminism, \/-, ~>}
import scala.language.{higherKinds, reflectiveCalls}
import scalaz.concurrent.Task
import Task._
import scala.util.Random

case class User(name: String, age: Int)

sealed trait UserOperation[T]
case class CreateUser(name: String, age: Int) extends UserOperation[User]

sealed trait AnalyticsOperation[T]
case class AnalyseUser(user: User) extends AnalyticsOperation[Int]

case class ExecStrategy[F[_], A](fa: F[A]) {
  val seq: Free[F, A] = Free.liftF(fa)
  val par: FreeAp[F, A] = FreeAp.lift(fa)
}

case class UserRepo[F[_]](implicit ev: Inject[UserOperation, F]) {
  def createUser(name: String, age: Int): ExecStrategy[F, User] =
    ExecStrategy[F, User](ev.inj(CreateUser(name, age)))
}
object UserRepo {
  implicit def toUserRepo[F[_]](implicit ev: Inject[UserOperation, F]): UserRepo[F] = UserRepo[F]
}

case class AnalyticsRepo[F[_]](implicit ev: Inject[AnalyticsOperation, F]) {
  def analyseUser(user: User): ExecStrategy[F, Int] =
    ExecStrategy[F, Int](ev.inj(AnalyseUser(user)))
}
object AnalyticsRepo {
  implicit def toAnalyticsRepo[F[_]](implicit ev: Inject[AnalyticsOperation, F]): AnalyticsRepo[F] = AnalyticsRepo[F]
}

object ProgramHelpers {
  type Program[F[_], A] = Free[FreeAp[F, ?], A]

  implicit class RichFree[F[_], A](free: Free[F, A]) {
    def asProgramStep: Program[F, A] = {
      implicit val m: Monad[Program[F, ?]] = Free.freeMonad[FreeAp[F, ?]]

      free.foldMap[ProgramHelpers.Program[F, ?]](new NaturalTransformation[F, ProgramHelpers.Program[F, ?]] {
        override def apply[A](fa: F[A]): Program[F, A] = liftFA(fa)
      })
    }
  }

  implicit class RichFreeAp[F[_], A](freeap: FreeAp[F, A]) {
    def asProgramStep: Program[F, A] = Free.liftF[FreeAp[F, ?], A](freeap)
  }

  def liftFA[F[_], A](fa: F[A]): Program[F, A] =
    Free.liftF[FreeAp[F, ?], A](FreeAp.lift(fa))
}

object InterpreterHelpers {
  type ProgramInstructions[A] = Coproduct[UserOperation, AnalyticsOperation, A]

  case class ParallelInterpreter[G[_]](f: ProgramInstructions ~> G)(implicit ev: Applicative[G]) extends (FreeAp[ProgramInstructions, ?] ~> G) {
    override def apply[A](fa: FreeAp[ProgramInstructions, A]): G[A] = fa.foldMap(f)
  }

  def combineInterpreters[F[_], G[_], H[_]](f: F ~> H, g: G ~> H): Coproduct[F, G, ?] ~> H =
    new (Coproduct[F, G, ?] ~> H) {
      override def apply[A](fa: Coproduct[F, G, A]): H[A] = fa.run match {
        case -\/(ff) => f(ff)
        case \/-(gg) => g(gg)
      }
    }

  implicit class RichNaturalTransformation[F[_], H[_]](val f: F ~> H) {
    def or[G[_]](g: G ~> H): Coproduct[F, G, ?] ~> H = combineInterpreters[F, G, H](f, g)
  }
}

object SlowUserInterpreter extends (UserOperation ~> Task) {
  override def apply[A](fa: UserOperation[A]): Task[A] = fa match {
    case CreateUser(name, age) =>
      Task {
        println(s"Creating user $name")
        Thread.sleep(5000)
        println(s"Finished creating user $name")
        User(name, age)
      }
  }
}

object SlowAnalyticsInterpreter extends (AnalyticsOperation ~> Task) {
  override def apply[A](fa: AnalyticsOperation[A]): Task[A] = fa match {
    case AnalyseUser(user) =>
      Task {
        println(s"Analysing user $user")
        Thread.sleep(2000)
        println(s"Finished analysing user $user")
        Random.nextInt(50)
      }
  }
}

// Runs 3 steps
// Step 1 - Create user Steve
// Step 2 - Create user Harriet
// Step 3 - Run analytics on steve and harriet in parallel
def program[F[_]](implicit userRepo: UserRepo[F],
                  analyticsRepo: AnalyticsRepo[F]): ProgramHelpers.Program[F, Int] = {
  import scalaz.syntax.applicative._
  import ProgramHelpers._
  for {
    user1 <- userRepo.createUser("steve", 23).seq.asProgramStep
    user2 <- userRepo.createUser("harriet", 33).seq.asProgramStep
    sumOfAnalytics <- (analyticsRepo.analyseUser(user1).par |@| analyticsRepo.analyseUser(user2).par)((a, b) => a + b).asProgramStep
  } yield sumOfAnalytics
}


// Runs 2 steps
// Step 1 - Create user Steve and harriet in parallel
// Step 2 - Run analytics on steve and harriet in parallel
def program2[F[_]](implicit userRepo: UserRepo[F],
                   analyticsRepo: AnalyticsRepo[F]): ProgramHelpers.Program[F, Int] = {
  import scalaz.syntax.applicative._
  import ProgramHelpers._
  for {
    users <- (userRepo.createUser("steve", 23).par |@| userRepo.createUser("harriet", 33).par)((u1, u2) => (u1, u2)).asProgramStep
    (user1, user2) = users
    sumOfAnalytics <- (analyticsRepo.analyseUser(user1).par |@| analyticsRepo.analyseUser(user2).par)((a, b) => a + b).asProgramStep
  } yield sumOfAnalytics
}

val parallelTaskApplicative = new Applicative[Task] {
  def point[A](a: => A) = Task.now(a)
  def ap[A,B](a: => Task[A])(f: => Task[A => B]): Task[B] = apply2(f,a)(_(_))
  override def apply2[A,B,C](a: => Task[A], b: => Task[B])(f: (A,B) => C): Task[C] =
    Nondeterminism[Task].mapBoth(a, b)(f)
}

//import ProgramHelpers._
import InterpreterHelpers._
val programInterpreter: ProgramInstructions ~> Task = SlowUserInterpreter or SlowAnalyticsInterpreter
program[ProgramInstructions].foldMap(ParallelInterpreter(programInterpreter)(parallelTaskApplicative)).unsafePerformSync
//Creating user steve
//Finished creating user steve
//Creating user harriet
//Finished creating user harriet
//Analysing user User(harriet,33)
//Analysing user User(steve,23)
//Finished analysing user User(harriet,33)
//Finished analysing user User(steve,23)


program2[ProgramInstructions].foldMap(ParallelInterpreter(programInterpreter)(parallelTaskApplicative)).unsafePerformSync
//Creating user harriet
//Creating user steve
//Finished creating user steve
//Finished creating user harriet
//Analysing user User(harriet,33)
//Analysing user User(steve,23)
//Finished analysing user User(harriet,33)
//Finished analysing user User(steve,23)

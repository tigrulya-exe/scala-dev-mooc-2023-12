package module4

import module4.homework.dao.entity.{Role, RoleCode, User}
import module4.homework.dao.repository.UserRepository
import module4.homework.services.UserService
import zio.{ZIO, ZLayer}
import zio.blocking.Blocking
import zio.test.Assertion._
import zio.test._

import java.util.UUID


object UserServiceSpec extends DefaultRunnableSpec{

    import MigrationAspects._
    val dc = DBTransactor.Ctx

    type Env = Blocking with TestContainer.Postgres with DBTransactor.DataSource with
        UserRepository.UserRepository with LiquibaseService.Liqui with  LiquibaseService.LiquibaseService with UserService.UserService
    
    val layer: ZLayer[Any, Throwable, Env] = 
        Blocking.live >+> TestContainer.postgres() >+> DBTransactor.test >+> LiquibaseService.liquibaseLayer ++ 
        UserRepository.live >+> UserService.live ++ LiquibaseService.live



    val zLayer: ZLayer[Any, Nothing, Env] = 
        layer.orDie


    val users = List(
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(120)),
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(120)),
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(120)),
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(120)),
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(120)),
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(120)),
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(120)),
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(120)),
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(120)),
        User(UUID.randomUUID().toString(), scala.util.Random.nextString(15), scala.util.Random.nextString(30), scala.util.Random.nextInt(120))
    )
    val usersGen = Gen.fromIterable(users)

    val Manager = RoleCode("manager")

    def spec = suite("UserServiceSpec")(
            testM("add user with role")(
                for{
                        userService <- ZIO.environment[UserService.UserService].map(_.get)
                        _ <- userService.addUserWithRole(users.head, Manager)
                        result <- userService.listUsersDTO()
                    } yield assert(result.length)(equalTo(1)) &&
                        assert(result.head.user)(equalTo(users.head)) && assert(result.head.roles)(equalTo(Set(Role(Manager.code, "Manager"))))
            )  @@ migrate(),
            testM("list user with role Manager should return empty List")(
                for{
                    userRepo <- ZIO.environment[UserRepository.UserRepository].map(_.get)
                    userService <- ZIO.environment[UserService.UserService].map(_.get)
                    _ <- userRepo.createUsers(users)
                    result <- userService.listUsersWithRole(Manager)
                } yield assert(result)(isEmpty)
            ) @@ migrate(),
            testM("list user with role Manager should return one Entry")(
                for{
                    userRepo <- ZIO.environment[UserRepository.UserRepository].map(_.get)
                    userService <- ZIO.environment[UserService.UserService].map(_.get)
                    _ <- userRepo.createUsers(users.tail)
                    _ <- userService.addUserWithRole(users.head, Manager)
                    result <- userService.listUsersWithRole(Manager)
                } yield assert(result.length)(equalTo(1)) && assert(result.head.user)(equalTo(users.head)) && 
                    assert(result.head.roles)(equalTo(Set(Role(Manager.code, "Manager"))))
            ) @@ migrate()
        ).provideCustomLayer(zLayer)  
}

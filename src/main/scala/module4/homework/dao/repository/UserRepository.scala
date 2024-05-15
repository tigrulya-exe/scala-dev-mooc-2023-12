package module4.homework.dao.repository

import zio.{Has, ULayer, ZIO, ZLayer}
import io.getquill.context.ZioJdbc._
import module4.homework.dao.entity.User
import module4.homework.dao.entity.{Role, UserToRole}
import module4.homework.dao.entity.UserId
import module4.homework.dao.entity.RoleCode
import module4.phoneBook.db


object UserRepository{


    val dc = db.Ctx
    import dc._

    type UserRepository = Has[Service]

    trait Service{
        def findUser(userId: UserId): QIO[Option[User]]
        def createUser(user: User): QIO[User]
        def createUsers(users: List[User]): QIO[List[User]]
        def updateUser(user: User): QIO[Unit]
        def deleteUser(user: User): QIO[Unit]
        def findByLastName(lastName: String): QIO[List[User]]
        def list(): QIO[List[User]]
        def userRoles(userId: UserId): QIO[List[Role]]
        def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit]
        def listUsersWithRole(roleCode: RoleCode): QIO[List[User]]
        def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]]
    }

    class ServiceImpl extends Service{


        private val userSchema = quote {
            query[User]
        }
        private val roleSchema = quote {
            query[Role]
        }
        private val userRoleSchema = quote {
            query[UserToRole]
        }

        override def findUser(userId: UserId): QIO[Option[User]] = dc.run(
            userSchema.filter(_.id == lift(userId.id))
        ).map(_.headOption)

        override def createUser(user: User): QIO[User] = dc.run(
            userSchema.insert(lift(user)).returning[User](quote(_))
        )

        override def createUsers(users: List[User]): QIO[List[User]] = dc.run(
            liftQuery(users)
              .foreach(user => userSchema.insert(user).returning[User](quote(_)))
        )

        override def updateUser(user: User): QIO[Unit] = dc.run(
            userSchema.filter(_.id == lift(user.id)).update(lift(user))
        ).unit

        override def deleteUser(user: User): QIO[Unit] = dc.run(
            userSchema.filter(_.id == lift(user.id)).delete
        ).unit

        override def findByLastName(lastName: String): QIO[List[User]] = dc.run(
            userSchema.filter(_.lastName == lift(lastName))
        )

        override def list(): QIO[List[User]] = dc.run(userSchema)

        override def userRoles(userId: UserId): QIO[List[Role]] = dc.run {
            for {
                userWithRoles <- userSchema
                userRole <- userRoleSchema.join(_.userId == userWithRoles.id)
                roles <- roleSchema.join(_.code == userRole.roleId)
            } yield roles
        }

        override def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit] = dc.run(
            userRoleSchema.insert(lift(UserToRole(roleCode.code, userId.id)))
        ).unit

        override def listUsersWithRole(roleCode: RoleCode): QIO[List[User]] = dc.run {
            for {
                userRole <- userRoleSchema.filter(_.roleId == lift(roleCode.code))
                users <- userSchema.join(_.id == userRole.userId)
            } yield users
        }

        override def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]] = dc.run(
            roleSchema.filter(_.code == lift(roleCode.code))
        ).map(_.headOption)
    }

    val live: ULayer[UserRepository] = ZLayer.succeed(new ServiceImpl)
}
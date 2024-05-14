package module4.homework.services

import zio.{Has, RIO, Task, URLayer, ZIO, ZLayer}
import module4.homework.dao.entity.User
import module4.homework.dao.entity.Role
import module4.homework.dao.repository.UserRepository
import zio.macros.accessible
import module4.homework.dao.entity.RoleCode
import module4.phoneBook.db

@accessible
object UserService{
    type UserService = Has[Service]

    trait Service{
        def listUsers(): RIO[db.DataSource, List[User]]
        def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]
        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]
    }

    class Impl(userRepo: UserRepository.Service) extends Service{
        val dc = db.Ctx
        import dc._

        def listUsers(): RIO[db.DataSource, List[User]] = userRepo.list()

        def listUsersDTO(): RIO[db.DataSource, List[UserDTO]] = for {
            users <- listUsers()
            userDtos <- toUserDtos(users)
        } yield userDtos

        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] = for {
            user <- userRepo.createUser(user)
            _ <- userRepo.insertRoleToUser(roleCode, user.typedId)
            userDto <- toUserDto(user)
        } yield userDto

        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource,List[UserDTO]] = for {
            users <- userRepo.listUsersWithRole(roleCode)
            userDtos <- toUserDtos(users)
        } yield userDtos

        private def toUserDtos(users: List[User]): RIO[db.DataSource, List[UserDTO]] = ZIO.collectAll {
            users.map(toUserDto)
        }

        private def toUserDto(user: User): RIO[db.DataSource, UserDTO] = for {
            roles <- userRepo.userRoles(user.typedId)
        } yield UserDTO(user, roles.toSet)
    }

    val live: URLayer[UserRepository.UserRepository, UserService] =
        ZLayer.fromService[UserRepository.Service, UserService.Service](userRepo => new Impl(userRepo))
}

case class UserDTO(user: User, roles: Set[Role])
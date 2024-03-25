package module3.zio_homework

import zio.{ExitCode, URIO}

object ZioHomeWorkApp extends zio.App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = runApp.exitCode
}

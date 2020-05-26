package cosc250.roboScala

import java.awt.Color

import akka.actor.Actor

import scala.util.Random
import scala.util.control.Breaks.{break, breakable}

class WallPatroler extends Actor {
  // Give our tank a unique name
  val name = s"Wall Patroler ${Random.alphanumeric.take(4).mkString}"

  // As soon as the tank is created, register it
  Main.gameActor ! Register(name, Color.pink)

  def receive:PartialFunction[Any, Unit] = {

    // Every game tick, we receive our tank state, and should send commands to
    // Main.gameActor to say what we want to do this tick
    case TankState(me) =>
      val nextPos = me.position + Vec2.fromRTheta(me.velocity, me.facing)
      if (GameState.inBounds(nextPos)) {
        Main.gameActor ! FullSpeedAhead
      } else {
        Main.gameActor ! TurnClockwise
      }

      // The turret should always try to aim at the center of the window
      val center = Vec2(GameState.width/2, GameState.height/2)
      val vectorToCenter = center - me.position
      val angleTurretMustFace = ((vectorToCenter.theta) + 2*Math.PI) % (2*Math.PI)
      val angleTurretIsFacing = ((me.turretFacing) + 2*Math.PI) % (2*Math.PI)

      if (angleTurretIsFacing > angleTurretMustFace && Math.abs(angleTurretIsFacing - angleTurretMustFace) < Math.PI) {
        Main.gameActor ! TurretAnticlockwise
      } else if (angleTurretIsFacing < angleTurretMustFace && Math.abs(angleTurretIsFacing - angleTurretMustFace) < Math.PI) {
        Main.gameActor ! TurretClockwise
      } else if (angleTurretIsFacing > angleTurretMustFace && Math.abs(angleTurretIsFacing - angleTurretMustFace) > Math.PI) {
        Main.gameActor ! TurretClockwise
      } else if (angleTurretIsFacing < angleTurretMustFace && Math.abs(angleTurretIsFacing - angleTurretMustFace) > Math.PI) {
        Main.gameActor ! TurretAnticlockwise
      }

      // the radar should always face the center
      val angleRadarMustFace = ((vectorToCenter.theta) + 2*Math.PI) % (2*Math.PI)
      val angleRadarIsFacing = ((me.radarFacing) + 2*Math.PI) % (2*Math.PI)

      if (angleRadarIsFacing > angleRadarMustFace && Math.abs(angleRadarIsFacing - angleRadarMustFace) < Math.PI) {
        Main.gameActor ! RadarAnticlockwise
      } else if (angleRadarIsFacing < angleRadarMustFace && Math.abs(angleRadarIsFacing - angleRadarMustFace) < Math.PI) {
        Main.gameActor ! RadarClockwise
      } else if (angleRadarIsFacing > angleRadarMustFace && Math.abs(angleRadarIsFacing - angleRadarMustFace) > Math.PI) {
      Main.gameActor ! RadarClockwise
      } else if (angleRadarIsFacing < angleRadarMustFace && Math.abs(angleRadarIsFacing - angleRadarMustFace) > Math.PI) {
        Main.gameActor ! RadarAnticlockwise
      }

      // ping radar when available
      if (me.energy == Tank.startingEnergy) {
        Main.gameActor ! RadarPing
      }

    case RadarResult(me, tanks) =>
      // If radar returns at least a single target we are in attacking state
      if (tanks.size > 0) {
        breakable {
          for (tank <- tanks) {
            if (tank.health > 0) {
              Main.gameActor ! Fire
              break
            }
          }
        }
      }
  }
}

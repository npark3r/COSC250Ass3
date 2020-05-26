package cosc250.roboScala

import java.awt.Color

import akka.actor.Actor

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.util.control.Breaks._


/*

--- TANK STRATEGY ---

While patrolling the tank has an equal probability of turning clockwise or anticlockwise and a much larger
probability to continue driving straight. This makes the tank have a random walk nature.

If the tank's next position would be out of bounds it turns anticlockwise. This is counter to the wall patrollers
 so that we may have a good chance of destroying them.

While patrolling the radar rotates between 45 degrees to the right of the direction the tank is facing and
45 degrees to the left. This is in order for us to detect tanks ahead of us. The turret will face forwards and not change
 direction while patrolling.

When the radar pings and it detects an enemy the target position is recorded and the tank moves towards this location.
The radar tries to stay on top of this target by rotating towards it. The turret turns to the location and when within
 a certain tolerance it fires.

 If the radar no longer pick up the target the tank returns to a patrolling state. If the target is destroyed the tank
 will also return to a patrolling state.

 */



class MyVeryOwnTank extends Actor {
  // Give our tank a unique name
  val name = s"MyVeryOwnTank ${Random.alphanumeric.take(4).mkString}"
  val frameMemory = 25
  // String indicating currently directional rotation of the radar while patrolling
  private var radarDirection:String = "Clockwise"
  val listOfIndicesUsedToFire = new ListBuffer[Int]
  listOfIndicesUsedToFire += 0


  // As soon as the tank is created, register it
  Main.gameActor ! Register(name, Color.pink)

  // A class to hold information about the tank's state
  case class tankState(queueID:Int = 1, state:String = "Patrolling", enemyLastSeen:Vec2 = Vec2(-1,-1), enemyFacing:Double = 0.0)

  private val queue:mutable.Queue[tankState] = mutable.Queue.empty[tankState]

  /** Pushes a state into the queue */
  def pushState(newTankState:tankState):tankState = {
    queue.enqueue(newTankState)

    // Drops a frame from the queue if we've reached the maximum number of frames to remember
    if (queue.lengthCompare(frameMemory) > 0) queue.dequeue()
    newTankState
  }

  // push opening state as patrolling with no enemy seen
  pushState(tankState())



  def receive:PartialFunction[Any, Unit] = {

    // Every game tick, we receive our tank state, and should send commands to
    // Main.gameActor to say what we want to do this tick
    case TankState(me) =>

      val nextPos = me.position + Vec2.fromRTheta(me.velocity, me.facing)
      val angleTankIsFacing = ((me.facing) + 2*Math.PI) % (2*Math.PI)
      val angleTurretIsFacing = ((me.turretFacing) + 2*Math.PI) % (2*Math.PI)
      val angleRadarIsFacing = ((me.radarFacing) + 2*Math.PI) % (2*Math.PI)

      // what to do if the tank might go out of bounds. Simply steer right


      if (queue.last.state == "Patrolling") { // Patrolling state

        // radar patrolling strategy
        if (angleTankIsFacing > Math.PI/4.0 && angleTankIsFacing < Math.PI * 7.0/4.0) { // Core logic apart from Pi/4 from sign change
          if (radarDirection == "Clockwise") {
            if ((angleTankIsFacing - angleRadarIsFacing) % (2.0 * Math.PI) < (-Math.PI / 4.0)) {
              //              System.out.println("changed direction to AC 1")
              radarDirection = "Anticlockwise"
            }
          } else if ( radarDirection == "Anticlockwise") {
            if ((angleTankIsFacing - angleRadarIsFacing) % (2.0*Math.PI)  > Math.PI/4) {
              //              System.out.println("changed direction to clockwise 1")
              radarDirection = "Clockwise"
            }
          }
        } else if (angleTankIsFacing < Math.PI/4.0) { //Bottom-right region
          if ( radarDirection == "Anticlockwise") {
            if (angleRadarIsFacing < (angleTankIsFacing + 7.0*Math.PI/4.0 ) && angleRadarIsFacing >= Math.PI) {
              //              System.out.println("changed direction to clockwise 2")
              radarDirection = "Clockwise"
            }
          } else if (radarDirection == "Clockwise") {
            if ((angleRadarIsFacing - angleTankIsFacing) % (2.0*Math.PI) > Math.PI/4.0 && angleRadarIsFacing < Math.PI) {
              //              System.out.println("changed direction to AC 2")
              radarDirection = "Anticlockwise"
            }
          }
        } else if (angleTankIsFacing > Math.PI * 7.0/4.0) { // Top-right region
          if ( radarDirection == "Clockwise") {
            if (angleRadarIsFacing > (Math.PI/4.0 - (2.0*Math.PI - angleTankIsFacing)) && angleRadarIsFacing <= Math.PI) {
              //              System.out.println("changed direction to AC 3")
              radarDirection = "Anticlockwise"
            }
          } else if (radarDirection == "Anticlockwise") {
            if (angleRadarIsFacing > Math.PI && (angleTankIsFacing - angleRadarIsFacing) % (2.0*Math.PI) > Math.PI/4.0) {
              //              System.out.println("changed direction to clockwise 3")
              radarDirection = "Clockwise"
            }
          }
        }

        // turret aiming. Turret should face same direction as tank if patrolling
        if (me.turretFacing < me.facing) {
          Main.gameActor ! TurretClockwise
        } else if (me.turretFacing > me.facing) {
          Main.gameActor ! TurretAnticlockwise
        }

        if (radarDirection == "Clockwise") {
          Main.gameActor ! RadarClockwise
        } else if (radarDirection == "Anticlockwise") {
          Main.gameActor ! RadarAnticlockwise
        }
      } else if (queue.last.state == "Attacking") { // Attacking state

        // Attacking movement - follow target
        val vectorToTarget = queue.last.enemyLastSeen - me.position
        val angleToTarget = ((vectorToTarget.theta) + 2*Math.PI) % (2*Math.PI)

        if (queue(queue.length - 2).state == "Patrolling") {
          System.out.println("Second last state was patting")
          // Turret turn to face target direction
          if (angleTurretIsFacing > angleToTarget && Math.abs(angleTurretIsFacing - angleToTarget) < Math.PI) {
            Main.gameActor ! TurretAnticlockwise
          } else if (angleTurretIsFacing < angleToTarget && Math.abs(angleTurretIsFacing - angleToTarget) < Math.PI) {
            Main.gameActor ! TurretClockwise
          } else if (angleTurretIsFacing > angleToTarget && Math.abs(angleTurretIsFacing - angleToTarget) > Math.PI) {
            Main.gameActor ! TurretClockwise
          } else if (angleTurretIsFacing < angleToTarget && Math.abs(angleTurretIsFacing - angleToTarget) > Math.PI) {
            Main.gameActor ! TurretAnticlockwise
          }

          // fire if within tolerance
          if(queue.last.queueID != listOfIndicesUsedToFire.last) {
            if (Math.abs(angleTurretIsFacing - angleToTarget) < 0.05) {
              if (me.canFire) {
                Main.gameActor ! Fire
                listOfIndicesUsedToFire += queue.last.queueID
              }
            }
          }

        } else if (queue(queue.length - 2).state == "Attacking") {
          System.out.println("Second last state was attacking")
          val timeDistanceToTarget = vectorToTarget.magnitude / Shell.velocity
          val vectorToFutureTarget = (queue.last.enemyLastSeen + (queue.last.enemyLastSeen - queue(queue.length - 2).enemyLastSeen) * timeDistanceToTarget * 20 - me.position)
          val futureGuessPos = queue.last.enemyLastSeen + (queue.last.enemyLastSeen - queue(queue.length - 2).enemyLastSeen) * timeDistanceToTarget

          System.out.println("timeDistanceToTarget is: " + timeDistanceToTarget)
          System.out.println("queue.last.enemyLastSeen is: " + queue.last.enemyLastSeen)
          System.out.println("queue(queue.length - 2).enemyLastSeen is: " + queue(queue.length - 2).enemyLastSeen)
          System.out.println("Future guessed position is: " + futureGuessPos)
          System.out.println("me.position is: " +me.position)
          System.out.println("vectorToFutureTarget is : " + vectorToFutureTarget)

          val angleToFutureTarget = ((vectorToFutureTarget.theta) + 2*Math.PI) % (2*Math.PI)

          if (angleTurretIsFacing > angleToFutureTarget && Math.abs(angleTurretIsFacing - angleToFutureTarget) < Math.PI) {
            Main.gameActor ! TurretAnticlockwise
          } else if (angleTurretIsFacing < angleToFutureTarget && Math.abs(angleTurretIsFacing - angleToFutureTarget) < Math.PI) {
            Main.gameActor ! TurretClockwise
          } else if (angleTurretIsFacing > angleToFutureTarget && Math.abs(angleTurretIsFacing - angleToFutureTarget) > Math.PI) {
            Main.gameActor ! TurretClockwise
          } else if (angleTurretIsFacing < angleToFutureTarget && Math.abs(angleTurretIsFacing - angleToFutureTarget) > Math.PI) {
            Main.gameActor ! TurretAnticlockwise
          }


          System.out.println("angleToFutureTarget is: " + angleToFutureTarget)
          // fire if within tolerance
          if(queue.last.queueID != listOfIndicesUsedToFire.last) {
            if (Math.abs(angleTurretIsFacing - angleToFutureTarget) < 0.05) {
              if (me.canFire) {
                Main.gameActor ! Fire
                listOfIndicesUsedToFire += queue.last.queueID
              }
            }
          }

        }

        // Radar turn to keep track of target
        if (angleRadarIsFacing > angleToTarget && Math.abs(angleRadarIsFacing - angleToTarget) < Math.PI) {
          Main.gameActor ! RadarAnticlockwise
        } else if (angleRadarIsFacing < angleToTarget && Math.abs(angleRadarIsFacing - angleToTarget) < Math.PI) {
          Main.gameActor ! RadarClockwise
        } else if (angleRadarIsFacing > angleToTarget && Math.abs(angleRadarIsFacing - angleToTarget) > Math.PI) {
          Main.gameActor ! RadarClockwise
        } else if (angleRadarIsFacing < angleToTarget && Math.abs(angleRadarIsFacing - angleToTarget) > Math.PI) {
          Main.gameActor ! RadarAnticlockwise
        }

        // ping radar when available on attack if we haven't fire recently
        if (me.energy == Tank.startingEnergy) {
          Main.gameActor ! RadarPing
        }


      }

      // Tank movement
      if (GameState.inBounds(nextPos)) {

        val randomPatrollingAction = Math.random() * 5
        if (randomPatrollingAction >= 0 && randomPatrollingAction < 0.5){
          Main.gameActor ! TurnAnticlockwise
        } else if (randomPatrollingAction >= 0.5 && randomPatrollingAction < 4.5) {
          Main.gameActor ! FullSpeedAhead
        } else {
          Main.gameActor ! TurnClockwise
        }
      } else {
        Main.gameActor ! TurnAnticlockwise
      }


      // ping radar when available
      if (me.energy == Tank.startingEnergy) {
        Main.gameActor ! RadarPing
      }

    case RadarResult(me, tanks) =>

      // If radar returns at least a single target we are in attacking state
      if (tanks.size > 0) {
        breakable { for (tank <- tanks) {
          if (tank.health > 0) {
            pushState(tankState(queueID = queue.last.queueID + 1, "Attacking", tanks.last.position, tanks.last.facing))
            System.out.println("enemy last seen: " + queue.last)
            break
          }
        }
          pushState(tankState(enemyLastSeen =  Vec2(-1,-1))) }


      } else if (tanks.size == 0) { // if radar result contains no tanks we patrol
        pushState(tankState(enemyLastSeen =  Vec2(-1,-1)))
      }


  }
}

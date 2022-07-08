package undertale.logic

abstract class CellType
case object Soul  extends CellType

trait DamageCell
trait Bone extends DamageCell

case object BoneVerticalTop extends CellType with Bone
case object BoneVerticalBot  extends CellType with Bone
case object BoneVerticalMid  extends CellType with Bone

case object BoneHorizontalLeft  extends CellType with Bone
case object BoneHorizontalRight  extends CellType with Bone
case object BoneHorizontalMid  extends CellType with Bone

case object BoneDamageBlock extends CellType with Bone

case object LaserHeadLeft  extends CellType
case object LaserHeadRight  extends CellType
case object LaserHeadUp  extends CellType
case object LaserHeadDown  extends CellType
case object Laser   extends CellType with DamageCell

case object Platform   extends CellType
case object Empty   extends CellType

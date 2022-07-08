package undertale.logic

case class Frame(graphic: List[List[CellType]], audio: List[String]) {
  def addAudio(name: String): Frame =
    copy(audio = this.audio :+ name)
}

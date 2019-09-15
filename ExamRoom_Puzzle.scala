class ExamRoom(_N: Int) {

  var occupied = mutable.TreeSet[Int](1, 2, 3, 5, 6, 7, 8, 9)

  def seat(): Int = {
    val occupied_list = occupied.toList
    if (occupied.isEmpty){
      occupied+=0
      0
    }
    else if (occupied_list.size==1){
      if (occupied_list(0)>(this._N-occupied_list(0))){
        occupied+=0
        0
      }
      else{
        occupied+=(this._N-1)
        (this._N-1)
      }
    }
    else{
      var cum_dif = mutable.TreeSet[Tuple2[Int,Int]]()
      for (i <- 0 to (occupied_list.size-2)){
        val pos = occupied_list(i)+(occupied_list(i+1)-occupied_list(i))/2
        cum_dif+=Tuple2(((occupied_list(i+1)-occupied_list(i))/2).toInt,pos)
      }
      cum_dif+=Tuple2((occupied_list(0)-0), 0)
      cum_dif+=Tuple2((this._N-1-occupied_list(occupied_list.size-1)), this._N-1)
      val cum_dif_list = cum_dif.toList
//          println(occupied_list)
      println(cum_dif_list)
      var prev_distance = None:Option[Int]
      var prev_node = None:Option[Int]
      breakable {
      for (i <- cum_dif_list.reverse){
          if((prev_distance.getOrElse(None)==None) || (prev_distance.getOrElse(None)==i._1)){
            prev_distance = Some(i._1)
            prev_node = Some(i._2)
          }
          else if (prev_distance.getOrElse(None)!=i._1){
//                println(i._1)
//                println("Break")
            break
          }
          else{
            println("Else",prev_node)
          }
        }
      }
      occupied+=prev_node.getOrElse(0)
//          println(occupied.toList)
      prev_node.getOrElse(0)
    }
  }

  def leave(p: Int) {
    occupied-=p
  }

}


val N = 10
val p =4
var obj = new ExamRoom(N)
println(obj.seat())
/*    println(obj.seat())
println(obj.seat())
obj.seat()
obj.seat()
obj.leave(p)
obj.seat()
*/

}

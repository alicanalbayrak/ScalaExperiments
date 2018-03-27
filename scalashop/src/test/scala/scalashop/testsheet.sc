import common.parallel

def execute(start: Int, end: Int): Unit = {
  if ((end - start) <= 2) {
    println(s"Executing call: $start - $end")
    //        parallel(blur(src, dst, start, (end - start) / 2, radius), blur(src, dst, (end - start) / 2, end , radius))
  } else {
    println(s"Rec call: $start - $end")
    parallel(execute(start, (end + start) / 2),execute((end + start) / 2 , end))
  }

}

execute(0, 8)
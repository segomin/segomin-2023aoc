


val s1 = Set((1, 2), (3, 4), (5, 6), (5, 7))

// Applying toMap method
val result = s1.groupBy(_._1).map { case (k, v) => (k, v.map(_._2))}

// Displays output
println(result)
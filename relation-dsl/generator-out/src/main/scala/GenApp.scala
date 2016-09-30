
package relation
import relation.shallow._


object GenApp {
  def main(args: Array[String]): Unit = Timing.time({
    val x2 = Schema.apply("number", "digit")
    val x39 = new RelationScanner("data/En.csv", '|')
    val x40 = RelationScanner.getNumLinesInFile("data/En.csv")
    val x41 = new Array[Array[String]](2)
    val x42 = new Range(0, 2, 1)
    val x50 = for(x47 <- 0 until 2) {
      val x48 = new Array[String](x40)
      val x49 = x41.update(x47, x48)
      x49
    }
    val x51 = new Range(0, x40, 1)
    val x77 = for(x65 <- 0 until x40) {
      val x66 = new Range(0, 2, 1)
      val x76 = for(x72 <- 0 until 2) {
        val x73 = x41.apply(x72)
        val x74 = x39.next_string()
        val x75 = x73.update(x65, x74)
        x75
      }
      x76
    }
    var x82: Int = 0
    val x83 = x41.apply(0)
    val x84 = x83.length
    val x85 = new Range(0, x84, 1)
    val x103 = for(x95 <- 0 until x84) {
      val x96 = x41.apply(0)
      val x97 = x96.apply(x95)
      val x102 = if((x97.==("one"))) 
      {
        val x99 = x82
        val x101 = x82 = (x99.+(1))
        x101
      }
      else
      {
        ()
      }
      
      x102
    }
    val x104 = new Array[Array[String]](2)
    val x105 = new Range(0, 2, 1)
    val x115 = for(x111 <- 0 until 2) {
      val x112 = x82
      val x113 = new Array[String](x112)
      val x114 = x104.update(x111, x113)
      x114
    }
    var x116: Int = 0
    val x117 = x41.apply(0)
    val x118 = x117.length
    val x119 = new Range(0, x118, 1)
    val x167 = for(x144 <- 0 until x118) {
      val x145 = x41.apply(0)
      val x146 = x145.apply(x144)
      val x166 = if((x146.==("one"))) 
      {
        val x148 = new Range(0, 2, 1)
        val x162 = for(x156 <- 0 until 2) {
          val x157 = x104.apply(x156)
          val x158 = x116
          val x159 = x41.apply(x156)
          val x160 = x159.apply(x144)
          val x161 = x157.update(x158, x160)
          x161
        }
        val x163 = x116
        val x165 = x116 = (x163.+(1))
        x165
      }
      else
      {
        ()
      }
      
      x166
    }
    val x170 = new Array[Int](1)
    val x171 = x170.update(0, 0)
    val x172 = x104.apply(0)
    val x174 = new Array[Array[String]](1)
    val x175 = new Range(0, 1, 1)
    val x189 = for(x183 <- 0 until 1) {
      val x184 = new Array[String]((x172.length))
      val x185 = x174.update(x183, x184)
      val x186 = x170.apply(x183)
      val x187 = x104.apply(x186)
      val x188 = x174.update(x183, x187)
      x188
    }
    var x190: String = ""
    val x191 = x174.apply(0)
    val x192 = x191.length
    val x193 = new Range(0, x192, 1)
    val x249 = for(x222 <- 0 until x192) {
      val x223 = x190 = ""
      val x224 = 1.-(1)
      val x225 = new Range(0, x224, 1)
      val x241 = for(x234 <- 0 until x224) {
        val x235 = x190
        val x236 = x174.apply(x234)
        val x237 = x236.apply(x222)
        val x238 = x235.+(x237)
        val x239 = x238.+("|")
        val x240 = x190 = x239
        x240
      }
      val x242 = x190
      val x243 = x174.apply(x224)
      val x244 = x243.apply(x222)
      val x245 = x242.+(x244)
      val x246 = x190 = x245
      val x247 = x190
      val x248 = println(x247)
      x248
    }
    ()
  }

, "Query Execution")}

/**
	Proof that a martingale betting system is ineffective over time (in roulette, at least)
*/
object Martingale {

	object Wheel{

		val gen = new scala.util.Random()

		val numbers = (for(i <- List.range(1, 37)) yield (if (i % 2 == 0) 'B' else 'R', i)) ++ List(('G', 0), ('G', 00))

		def spin() : (Char, Int) = numbers(gen.nextInt(38))
	}

	class Session(bankroll: Int, origBetSize: Int){

		def bet(bankroll: Int, betSize: Int, result: List[Map[String, Any]]): List[Map[String, Any]] = {
			if (result.length > 0) println(result(result.length-1))
			if(bankroll == 0 ) return result
			val newbankroll = bankroll - betSize
			val spin = Wheel.spin()
			return spin match {
				case ('B', _) => bet(newbankroll + betSize*2, 
									origBetSize, 
									result :+ Map("bet" -> betSize, "spin" -> spin, "bankroll" -> (newbankroll + betSize*2)))
				case (_, _) => bet(newbankroll, 
									if(betSize*2 > newbankroll) origBetSize else betSize*2, 
									result :+ Map("bet" -> betSize, "spin" -> spin, "bankroll" -> newbankroll))
			}
		}

		val bets = bet(bankroll, origBetSize, List())

	}

	object Session {

		def apply(bankroll: Int, origBetSize: Int): Session = {
			return new Session(bankroll: Int, origBetSize: Int)
		}
	}


	def main(args:Array[String]) = {
		print("Enter Bankroll: ")
		val bankroll = Console.readInt()
		print("Enter starting bet size: ")
		val betSize = Console.readInt()
		//print("Enter number of sessions: ")
		//val numSessions = Console.readInt()
		val sess = Session(bankroll, betSize)
		//println(sess.bets);
	}
}
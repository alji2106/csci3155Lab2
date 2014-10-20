1.)
	a.)
		_________________			_________________
		b within VObjects			a within VObjects 

		   A within AObjects	 	 V within AObjects
		======================		===================
		A && A within AObjects		a,b within AObjects
	b.)
		ex.) aba
				A 							A
			  /   \						  /   \
			 A  &  A 					 A  &  A
			/	 /   \				   /   \     \
		   V    A  &  A 			  A  &   A    V
		  /	   /     /				 /		/	 /	
		 a    V     V 				V      V    a
		 	 /     /			   /      /
		 	b     a 			  a 	 b

		 	As shown in the above trees, there are multiple ways to define this statement
		 	making this grammar ambiguous.
	c.)
		A list of charactors including a, b, e, and c. The words made are either a combination
		of the three or all of one type.
	d.)
		1.)  baab
				S => AaBb => baBb => baab
		4.)  bbaab
				S => AaBb => AbaBb => bbaBb => bbaab
	e.)
		1.)	 abcd
				S
			  / | \ \
			 a  S  c B
			 	|	  \
			 	b 	   d
2.)
	a.)
		i.) e is either an operand (like 2), a recursion of itself with an additon of an operator (like +) and another operand (like 3.1), or the same thing as the above statements with the addition of machine epsilon.
		ii.) These are one in the same grammar the only difference being is the second grammar has machine epsilon in it. 
	b.)
	//http://en.wikipedia.org/wiki/Operator-precedence_parser
		def getPres(e: expr): num = e match {
			case - => 2
			case >> => 1
			case _ => 0
		} 
		def pres(e: expr): expr = {
			def eval(lhs, p) : expr = {  //lhs = left hand side
				while(expr.next is a binary operator && getPres(next token) > p) {
					op := expr.next
					rhs := e  //right hand side
					pres := getPres(op)
					while((op.next is binary op && getPres(op.next) > pres) || 
					    (right assos op whose preseendence == pres) {

					    loookahead := op.next
					    pres4look := getPres(lookahead)
					    rhs := eval (rhs, pres4look)
					}
				    lhs := result of applying op with operands lhs and rhs
				}
				return lhs
			}
			eval(e, 0)
		}
	c.)
		Num ::= W.F | W.FEW
		W 	::= Wn | m | -m 		where n is a number 0-9 and m is a number 1-9
		F   ::= nF | m

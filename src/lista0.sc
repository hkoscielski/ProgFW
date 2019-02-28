//Hubert Kościelski
def last[A](xs:List[A]):A =
    if (xs== Nil)
        throw new NoSuchElementException("Empty list")
    else if (xs.tail == Nil)
	    xs.head
    else
        last(xs.tail)

last(List(1,2,3,5)) == 5
last(List(1,1,1,1)) == 1
last(List(1)) == 1
last(List())
object question1 extends App{

    // tational class take two parameters as enumerator and denominator
    class Rational(n:Int, d:Int) {
        require(d > 0, "denominator must be greater than 0") // prompt user if denominator is negative or zero
        // methods to get the denominator and enumerator
        def enumerator:Int = n
        def denominator:Int = d
        
        // negation method
        // new rational number with negative enumerator
        def neg = new Rational(-this.enumerator, this.denominator)
        // to print the number we've to override the toString method
        override def toString(): String = enumerator + "/" + denominator
    }
    
    val obj = new Rational(7,8)
    println(obj.neg)
}
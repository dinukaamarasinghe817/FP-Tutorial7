object question2 extends App{

    // same rational class above but with addition method and simplify method and - operator
    class Rational(n:Int, d:Int) {
        require(d > 0, "denominator must be greater than 0")
        def enumerator:Int = n
        def denominator:Int = d

        // if the rational number can further simplify then simplify
        def simplify(r:Rational) = r.enumerator match{
            case x if(x>0) => new Rational(r.enumerator/gcd(r.enumerator,r.denominator), r.denominator/gcd(r.enumerator,r.denominator))
            case x if(x<0) => (new Rational(r.enumerator.abs/gcd(r.enumerator,r.denominator), r.denominator/gcd(r.enumerator.abs,r.denominator))).neg
        }

        // new rational number with enumerator and denominator as below
        // enumerator = first.enumerator x second.denominator + first.denominator x second.enumerator
        // denominator = first.denominator x second.denominator
        // affter adding simplify the result
        def add(r:Rational) = this.simplify(new Rational(this.enumerator*r.denominator + this.denominator*r.enumerator, this.denominator*r.denominator))

        def neg = new Rational(-this.enumerator, this.denominator)

        override def toString(): String = enumerator + "/" + denominator

        // minus operator on a object
        def -(r:Rational) = this.add(r.neg)
    }

    // gcd method to simplify rational numbers further
    def gcd(a: Int,b: Int): Int = {
       if(b ==0) a else gcd(b, a%b)
    }

    val x = new Rational(3,4)
    val y = new Rational(5,8)
    val z = new Rational(2,7)
  
    val a = x-y-z
    
    println(a)
}
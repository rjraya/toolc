//Expected output: 14/9
program Example6 {
    println(new Frac().init(14,9).toString());
}
class Frac {
    var numerator : Int;
    var denominator : Int;
    var sign : Bool; // true means positive.
    var util : Util;

    def init(n : Int, d : Int) : Frac = {
        util = new Util();

        numerator = util.abs(n);
        denominator = util.abs(d);
        sign = (n < 0 && d < 0 || (0 < n || n == 0) && (0 < d || d == 0));
        return this.simplify();
    }

    def getNumerator() : Int = {
        return numerator;
    }

    def getDenominator() : Int = {
        return denominator;
    }

    def setPos(positive : Bool) : Frac = {
        sign = positive;
        return this;
    }

    def isPos() : Bool = {
        return sign;
    }

    def simplify() : Frac = {
        var gcd_ : Int;

        if(!(numerator == 0) && !(denominator == 0)) {
            gcd_ = util.gcd(numerator, denominator);

            if(!(gcd_ == 1)) {
                numerator = numerator / gcd_;
                denominator = denominator / gcd_;
            }
        }

        return this;
    }

    def plus(other : Frac) : Frac = {
        var lcm : Int;
        var lfac : Int;
        var rfac : Int;

        lcm = util.lcm(denominator, other.getDenominator());
        lfac = lcm / denominator;

        if(!sign) {
            lfac = 0 - lfac;
        }

        rfac = lcm / other.getDenominator();

        if(!other.isPos()) {
            rfac = 0 - rfac;
        }

        return (new Frac()).init((lfac * numerator) + (rfac * other.getNumerator()), lcm);
    }

    def minus(other : Frac) : Frac = {
        return this.plus(other.negative());
    }

    def times(other : Frac) : Frac = {
        return (new Frac()).init(numerator * other.getNumerator(), denominator * other.getDenominator()).simplify().setPos(this.isPos() && other.isPos() || !this.isPos() && !other.isPos());
    }

    def divided(other : Frac) : Frac = {
        return this.times(other.inverse());
    }

    def inverse() : Frac = {
        return (new Frac()).init(denominator, numerator);
    }

    def negative() : Frac = {
        return (new Frac()).init(numerator, denominator).setPos(false);
    }

    def toString() : String = {
        var result : String;
        if(sign) {
            result = "";
        } else {
            result = "-";
        }
        return result + numerator + "/" + denominator;
    }
}
class Util {
    def abs(v : Int) : Int = {
        var res : Int;

        if(!(v < 0)) {
            res = v;
        } else {
            res = 0 - v;
        }
        return res;
    }

    def gcd(m_ : Int, n_ : Int) : Int = {
        var t : Int;
        var r : Int;
        var result : Int;
        var m : Int;
        var n : Int;

        m = this.abs(m_);
        n = this.abs(n_);

        if (m < n) {
            t = m;
            m = n;
            n = t;
        }

        r = this.mod(m,n); // m % n;

        if (r == 0) {
            result = n;
        } else {
            result = this.gcd(n, r);
        }
        return result;
    }

    def lcm(m : Int, n : Int) : Int = {
        return (n*m) / this.gcd(n,m);
    }

    def mod(m : Int, n : Int) : Int = {
        return m - (n * (m / n));
    }
}
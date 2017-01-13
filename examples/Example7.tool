program Example8{
    println(1);
}

// This class contains the array of integers and
// methods to initialize, print and sort the array
// using Quicksort
class QS {
    var number : Int[];
    var size : Int;

    def Sort(left : Int, right : Int) : Int = {
            var v : Int;
            var i : Int;
            var j : Int;
            var nt : Int;
            var t : Int;
            var cont01 : Bool;
            var cont02 : Bool;
            var aux03 : Int;

            t = 0 ;
            if (left < right){
                v = number[right] ;
                i = left - 1 ;
                j = right ;
                cont01 = true ;
                while (cont01){
                    cont02 = true ;
                    while (cont02){
                        i = i + 1 ;
                        aux03 = number[i] ;
                        if (!(aux03<v)) cont02 = false ;
                        else cont02 = true ;
                    }
                    cont02 = true ;
                    while (cont02){
                        j = j - 1 ;
                        aux03 = number[j] ;
                        if (!(v < aux03)) cont02 = false ;
                        else cont02 = true ;
                    }


                    t = number[i] ;
                    number[i] = number[j] ;
                    number[j] = t ;
                    //aux03 = i + 1 ;
                    if ( j < (i+1)) cont01 = false ;
                    else cont01 = true ;
                }
                number[j] = number[i] ;
                number[i] = number[right] ;
                number[right] = t ;
                nt = this.Sort(left,i-1);
                nt = this.Sort(i+1,right);
            }
            else nt = 0 ;
            return 0 ;
        }
}
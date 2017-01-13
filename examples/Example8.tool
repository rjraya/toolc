program Example8 {
    println(1);
}

// This class contains an array of integers and
// methods to initialize, print and search the array
// using Binary Search
class Ex8 {
    var number : Int[];
    var size : Int;

    def Search(num : Int) : Bool = {
        var right : Int;
        var left : Int;
        var var_cont : Bool;
        var medium : Int;
        var nt : Int;

        right = number.length;
        right = right - 1;
        left = 0;
        var_cont = true;
        while (var_cont) {
          medium = left + right;
          if (num < right)
            right = medium - 1;
          else
            left = medium + 1;
          if (right < left)
            var_cont = false;
          else
            nt = 0;
        }
        return false;
    }
}

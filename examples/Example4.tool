//Expected output: 17
program Example4 {
    println(new Ex4().Init(10));
}

class Ex4 {
    var number : Int[];
    var size : Int;

    // Initialize array of integers
    def Init(sz : Int) : Int = {
        var i : Int;

        size = sz ;
        i = 3+4;
        size = size + i;

        return size ;
    }
}
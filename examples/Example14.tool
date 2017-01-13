program Example14 {
    println(new Ex14().Init(10));
}

class Ex14 {
    var size : Int;

    def Init(sz : Int) : Int = {
        var i : Int;
        size = sz ;
        i = 3+4;
        size = size + i;

        return size ;
    }
}
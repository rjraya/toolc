program Example9{
 println(new Ex9().met());
}

class Ex9{
  def met() : Int = {
      var newarr : Int[];
      var i : Int;
      var arr : Int[];
      var size : Int;

      size = 8;

      arr = new Int[size];
      newarr = new Int[arr.length];

      i = 0;
      while(i < newarr.length) {
        newarr[i] = 0 - 1;
        i = i + 1;
      }

      return 0;
    }
}
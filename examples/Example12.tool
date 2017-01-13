program Example12{
 println(new Ex12().met());
}

class Ex12{
  def met() : Int = {
      var newarr : Int[];
      var i : Int;
      var arr : Int[];

      arr = new Int[8];
      newarr = new Int[arr.length];

      i = 0;
      while(i < newarr.length) {
        newarr[i] = 0 - 1;
        i = i + 1;
      }

      return 0;
    }
}
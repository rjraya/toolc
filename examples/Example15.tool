program Example15{
 println(new Ex15().met());
}

class Ex15{
  def met() : Int = {
      var newarr : Int[];
      var arr : Int[];
      arr = new Int[1];
      newarr = arr;
      println(newarr == arr);
      return 0;
    }
}
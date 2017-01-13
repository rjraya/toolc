program Example16{
 println(new Ex16().met());
}

class Ex16{
  def met() : String = {
      var arr : Int[];
      var c : String;
      var str : String;

      str = "00";
      arr = new Int[1];
      arr[0] = 1;

      if(arr[0] == 1)
       c = "88";
      else
       c = "99";
      str = str + c;
      return str;
    }
}
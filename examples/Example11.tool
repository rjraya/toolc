program Example11{
 println(new Ex11().printing());
}

class Ex11{
  def printing() : Int = {
    var z : Int;
    z = this.printing3(this.printing1(),this.printing2());
    return z;
  }
  def printing1() : Int = {
    println("printing1 executed");
    return 1;
  }

  def printing2() : Int = {
    println("printing2 executed");
    return 2;
  }

  def printing3(x1:Int,x2:Int) : Int = {
    return x1+x2;
  }
}
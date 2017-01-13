//This example tests for side effects of method calls
program Example10{
 println(new Ex10().printing());
}

class Ex10{
  def printing() : Int = {
    var i : Int;
    var j : Int;
    var z : Int;

    i = this.printing1();
    z = i;
    /*j = this.printing2();*/
    println(z);

    return i;
  }
  def printing1() : Int = {
    println("printing1 executed");
    return 1;
  }

  /*def printing2() : Int = {
    println("printing2 executed");
    return 2;
  }*/
}
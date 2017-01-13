program Inheritance{
   println(new Conversation().start());
}

class Conversation{
  var p: Person;
  var t: Teacher;
  var s: Student;
  var downcast: Person;
  //var upcast: Teacher;

  def start() : Int = {
    downcast=new Teacher();
    //upcast= new Person(); 
  
    p = new Person();
    t = new Teacher();
    s = new Student();
  
    do(p.setName("Juan"));
    do(t.setName("Pepe"));
    do(s.setName("Luis"));
    do(p.talk());
    println(" my name is " + p.getName());
    do(t.talk());
    println(" my name is " + t.getName());
    do(s.talk());
    println(" my name is " + s.getName());
  
    return 0;
  }
}

class Person{
  var id : String;
  var name : String;

  def getName() : String = { return name; }
  def getId() : String = { return id; }
  def setName(n : String) :Int = { name = n; return 0;}
  def setId(i : String) : Int = { id = i; return 0;}
  def talk() : Int = { println("bla bla bla"); return 0; }
}

class Teacher extends Person{
  var subject : String;
  var experience : Int;

  def setSubject(sub : String) : Int = {subject = sub; return 0;}
  def setExperience(exp : Int) : Int = {experience = exp; return 0;}
  def getSubject() : String = { return subject; }
  def getExperience() : Int = { return experience; }
  def talk() : Int = { println("bla bla bla " + "I'm a teacher"); return 0; }
}

class Student extends Person{
  var year : Int;
  var section : String;

  def setYear(y : Int) :Int = {year = y; return 0;}
  def setSection(sec : String) :Int = {section = sec; return 0;}
  def getYear() : Int = { return year; }
  def getSection() : String = { return section; }
  def talk() : Int = { println("bla bla bla " + "I'm a student"); return 0; }
}
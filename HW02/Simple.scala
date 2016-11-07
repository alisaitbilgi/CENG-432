object Simple extends App{
    var myList = Array(10, 25, 30)
    myList.foreach((element) =>
      if (element % 2 == 0)
        println(element * 2)
      else
        println(element * 3)
    )
}
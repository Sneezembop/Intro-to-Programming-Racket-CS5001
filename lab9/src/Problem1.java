/**
 * Created by Miles on 11/28/2016.
 */
   /** ;; to represent a person
    ;; A Person is (make-person String Number String)
    (define-struct (person name age gender))

    (define tim (make-person "Tim" 20 "M"))
    (define pat (make-person "Pat" 19 "F"))
    (define kim (make-person "Kim" 17 "F"))
    (define dan (make-person "Dan" 22 "M"))

    */
   class Address{
       String city;
       String state;

       Address (String city, String state){
           this.city = city;
           this.state = state;
       }

   }
   class Person{

       String name;
       int age;
       String gender;
       Address address;

       Person (String name, int age, String gender, Address address){
           this.name = name;
           this.age = age;
           this.gender = gender;
           this.address = address;
       }
   }


class ExamplesPerson{

    Address boston = new Address ("Boston", "MA");
    Address warwick = new Address ("Warwick", "RI");
    Address nashua = new Address ("Nashua", "NH");

    Person tim = new Person("Tim", 20, "M", warwick);
    Person pat = new Person ("Pat", 19, "F", boston);
    Person kim = new Person ("Kim", 17, "F", boston);
    Person dan = new Person ("Dan", 22, "M", nashua);


}



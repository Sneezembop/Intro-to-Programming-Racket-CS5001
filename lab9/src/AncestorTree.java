/**
 * Created by Miles on 11/28/2016.
 */
interface AncestorTree {

    /** ;; An Ancestor Tree (AT) is one of
     ;; -- unknown
     ;; -- (make-tree Person AT AT)

     ;; A Person is defined as above

     */

}

class Unknown implements AncestorTree{

}

class MakeTree implements AncestorTree{
    Person person;
    AncestorTree left;
    AncestorTree right;

    MakeTree(Person person, AncestorTree left, AncestorTree right){
        this.person = person;
        this.left = left;
        this.right = right;

    }

}

class ATExamples {

    Person tim = new Person("Tim", 20, "M", new Address("Boston", "MA"));
    Person jane = new Person ("Jane", 50, "F", new Address("Boston", "MA"));
    Person mike = new Person ("Mike", 55, "M", new Address("New York City", "NY"));
    Person jim = new Person("Jim", 85, "M", new Address("New York City", "NY"));

    MakeTree timTree = new MakeTree(tim,
            new MakeTree(jane, new Unknown(), new Unknown()),
            new MakeTree(mike,
                    new MakeTree(jim, new Unknown(), new Unknown()), new Unknown()
            )
    );


}
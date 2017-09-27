/**
 * Created by Miles on 11/28/2016.
 */
interface DeliItem {

}


class Soup implements DeliItem{
    int price;
    boolean vegi;

    Soup(int price, boolean vegi){
        this.price = price;
        this.vegi = vegi;
    }
}

class Salad implements DeliItem{
    int price;
    boolean vegi;
    String dressing;

    Salad(int price, boolean vegi, String dressing){
        this.price = price;
        this.vegi = vegi;
        this.dressing = dressing;
    }

}

class Sandwich implements DeliItem{
    int price;
    boolean vegi;
    String filling1;
    String filling2;
    String bread;

    Sandwich(int price, boolean vegi, String bread, String filling1){
        this.price = price;
        this.vegi = vegi;
        this.bread = bread;
        this.filling1 = filling1;
    }
    Sandwich(int price, boolean vegi, String bread, String filling1, String filling2){
        this.price = price;
        this.vegi = vegi;
        this.bread = bread;
        this.filling1 = filling1;
        this.filling2 = filling2;
    }

}


class ExampleDeli{

    Soup minestrone = new Soup(500, true);
    Soup chickennoodle = new Soup(650, false);

    Salad cesar = new Salad(700, false, "Cesar");
    Salad house = new Salad(550, true, "Italian");

    Sandwich club = new Sandwich(800, false, "Rye", "Ham", "Turkey");
    Sandwich blt = new Sandwich(800, false, "Sourdough", "Bacon");


}
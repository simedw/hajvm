public class Main {
    public int value;
    public Main(int v) {
       value = v;
       System.out.println("You called me, mortal"); 
    }

    public void print() {
        for (int i = 0; i < value; i++)
            System.out.println("printing from main");
    }

    public void inc() {
        value = value + 1;
    }

    public static int fac(int f) {
        if(f <= 1)
            return 1;
        return f * fac(f-1);
    }

    public static void main(String[] args) {
        System.out.println("Hello World!");
        Main m1 = new Main(2);
        Main m2 = new Main(3);

        System.out.println("m1:");
        m1.print();
        System.out.println("m2:");
        m2.print();
        m1.inc();
        m2.inc();
        System.out.println("m1:");
        m1.print();
        System.out.println("m2:");
        m2.print();

    }
}

public class Main {

    public static int fac(int f) {
        if(f <= 1)
            return 1;
        return 2;
    }

    public static void main(String[] args) {
        int a = 0;
        System.out.println("Hello World!");
        for(int i = 0; i < fac(2); i++)
            System.out.println("I'm alive");
        System.out.println("Done");
    }
}

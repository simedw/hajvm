public class Main {

    public static int fac(int f) {
        if(f <= 1)
            return 1;
        return f * fac(f-1);
    }

    public static void main(String[] args) {
        System.out.println("Hello World!");
        for(int i = 0; i < fac(3); i++)
            System.out.println("I'm alive");
    }
}

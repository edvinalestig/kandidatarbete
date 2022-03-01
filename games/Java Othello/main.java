import java.util.Arrays;
import java.util.Scanner;

public class main {

    public static void main(String[] args) throws Exception { new main().program(args); }

    private void program(String[] args) throws Exception {

        int turnCounter = 4;
        int player = 1;
        boolean success = false;
        int Xtmp = 0;
        int Ytmp = 0;
        Othello othello = new Othello(args[0], args[1], Integer.parseInt(args[2]));
        Scanner sc = new Scanner(System.in);

        while(turnCounter < (Integer.parseInt(args[2]) * Integer.parseInt(args[2]))){

            printBoard(othello.getBoard());

            System.out.println();
            System.out.println("Player: " + player);

            System.out.println("Place at Y-Coordinate?");
            Ytmp = sc.nextInt();
            System.out.println("Place at X-Coordinate?");
            Xtmp = sc.nextInt();
            success = othello.placeGamePiece(Xtmp - 1, Ytmp - 1, player);

            if(success && (player != 1)){
                turnCounter++;
                player = 1;
                success = false;
            }
            else if(success && (player != 2)){
                turnCounter++;
                player = 2;
                success = false;
            }

        }
        printBoard(othello.getBoard());
        System.out.println("Results");
        System.out.println(args[0] + ": " + othello.calculateScore(1));
        System.out.println(args[1] + ": " + othello.calculateScore(2));

    }

    private void printBoard(int[][] input){
        for(int i = 0; i < input.length; i++){
            System.out.println(Arrays.toString(input[i]));
        }
    }
}

public class Othello {

    private int[][] board;
    private String player1;
    private String player2;

    /**
     * Constructor for initializing Board and for
     * Placing the initial 4 pieces in the middle of the board.
     */

    public Othello(String player1, String player2, int size) {
        this.board = new int[size][size];
        board[(board.length / 2) - 1][(board.length / 2) - 1] = 2;
        board[(board.length / 2) - 1][(board.length / 2)] = 1;
        board[(board.length / 2)][(board.length / 2) - 1] = 1;
        board[(board.length / 2)][(board.length / 2)] = 2;
        this.player1 = player1;
        this.player2 = player2;
    }

    public int[][] getBoard() { return this.board; }

    public String getPlayer1() {
        return this.player1;
    }

    public String getPlayer2() {
        return this.player2;
    }

    /**
     * Checks and updates board first in the X-direction then
     * the Y-direction if some of the gamespieces needs to
     * be turned.
     */

    private void updateBoard(int X, int Y, int player) throws Exception {
        int[] tmp = checkDirection('H', X, Y, player);
        changeGamePieces(X, Y, 'H', tmp, player);
        tmp = checkDirection('V', X, Y, player);
        changeGamePieces(X, Y, 'V', tmp, player);
    }

    /**
     * Checks in the X and Y direction if there are any of the opponents pieces
     * between the current players placed piece and another piece placed
     * By the same player.
     */

    private int[] checkDirection(char direction, int X, int Y, int player) throws Exception {

        int[] tmp = new int[2];

        switch (direction) {
            case 'H' -> {
                if (X != 0) {
                    for (int i = (X - 1); i >= 0; i--) {
                        if ((board[Y][i] != 0) && (board[Y][i] != player)) {
                            tmp[0] = tmp[0] + 1;
                        } else {
                            if(board[Y][i] != player){
                                tmp[0] = 0;
                            }
                            break;
                        }
                    }
                }
                if (X != (board[Y].length - 1)) {
                    for (int j = (X + 1); j < board[Y].length; j++) {
                        if ((board[Y][j] != 0) && (board[Y][j] != player)) {
                            tmp[1] = tmp[1] + 1;
                        } else {
                            if(board[Y][j] != player){
                                tmp[1] = 0;
                            }
                            break;
                        }
                    }
                }
            }

            case 'V' -> {
                if (Y != 0) {
                    for (int i = (Y - 1); i >= 0; i--) {
                        if ((board[i][X] != 0) && (board[i][X] != player)) {
                            tmp[0] = tmp[0] + 1;
                        } else {
                            if(board[i][X] != player){
                                tmp[0] = 0;
                            }
                            break;
                        }
                    }
                }
                if (Y != (board.length - 1)) {
                    for (int j = (Y + 1); j < board[Y].length; j++) {
                        if ((board[j][X] != 0) && (board[j][X] != player)) {
                            tmp[1] = tmp[1] + 1;
                        } else {
                            if(board[j][X] != player){
                                tmp[1] = 0;
                            }
                            break;
                        }
                    }
                }
            }
            case default -> {
                throw new Exception("illegalAction");
            }

        }
        return tmp;
    }

    /**
     * Changes the pieces detected by the checkDirection method if there
     * are any pieces that have been "locked" in by the current player.
     */

    private void changeGamePieces(int startX, int startY, char direction, int[] nrsteps, int player) throws Exception {

        switch (direction) {
            case 'H' -> {
                for (int i = (startX - nrsteps[0]); i <= (startX + nrsteps[1]); i++) {
                    board[startY][i] = player;
                }
                break;
            }
            case 'V' -> {
                for (int j = (startY - nrsteps[0]); j <= (startY + nrsteps[1]); j++) {
                    board[j][startX] = player;
                }
                break;
            }
            case default -> {
                throw new Exception("illegalAction");
            }
        }

    }

    /**
     * Method for placing a game piece if possible on the board
     * then updating the board accordingly.
     * Returns true on success otherwise false.
     */

    public boolean placeGamePiece(int X, int Y, int player) throws Exception {

        if ((board[Y][X] == 0) && checkValidMove(X, Y, player)) {
            board[Y][X] = player;
            updateBoard(X, Y, player);
            return true;
        } else {
            return false;
        }
    }

    /**
     * Calculates the current score for the given player.
     */

    public int calculateScore(int player) {

        int result = 0;

        for (int i = 0; i < board.length; i++) {
            for (int j = 0; j < board[i].length; j++) {
                if (board[i][j] == player) {
                    result++;
                }
            }
        }
        return result;
    }

    /**
     * Checks if the current move is a valid move accordingly to
     * the rules specified by othello.
     * Returns true if valid otherwise false.
     */

    private boolean checkValidMove(int Xpos, int YPos, int player) {

        if((Xpos > 0) && ((board[YPos][Xpos - 1]) != 0) && ((board[YPos][Xpos - 1]) != player)){
            return true;
        }
        else if((Xpos < (board.length - 2)) && ((board[YPos][Xpos + 1]) != 0) && ((board[YPos][Xpos + 1]) != player)){
            return true;
        }
        else if((YPos > 0) && ((board[YPos - 1][Xpos]) != 0) && ((board[YPos - 1][Xpos]) != player)){
            return true;
        }
        else return (YPos < (board.length - 2)) && ((board[YPos + 1][Xpos]) != 0) && ((board[YPos + 1][Xpos]) != player);
    }
}
package sample;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;
import protocols.Rational;

public class Main extends Application {

    @Override
    public void start(Stage primaryStage) throws Exception{
        Parent root = FXMLLoader.load(getClass().getResource("sample.fxml"));
        primaryStage.setTitle("Hello World");
        primaryStage.setScene(new Scene(root, 300, 275));
        Rational.RationalExpr a = new Rational.RationalExpr(1, 3);
        Rational.RationalExpr b = new Rational.RationalExpr(3, 1);
        System.out.println(Rational.RationalExpr$.MODULE$.gcd(3, 4));
        System.out.println("" + a + b.reciprocal() + a.$div(b));
        primaryStage.show();
    }


    public static void main(String[] args) {
        if(args.length == 1 && args[0].toLowerCase().equals("repl")) {
            Expr.main(args);
            System.exit(0);
        } else launch(args);
    }
}

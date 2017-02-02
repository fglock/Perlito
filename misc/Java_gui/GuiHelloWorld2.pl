 
# Example GUI application using JavaFX
#
# from the main Perlito directory:
# $ perl perlito5.pl -Isrc5/lib -Imisc/Java_gui -Cjava misc/Java_gui/GuiHelloWorld2.pl > MainGui.java
# $ javac MainGui.java
# $ java MainGui
#

use JavaGui;

sub action {
    my ($self, $param) = @_;
    my Stage $primaryStage = $param->to_Stage();

    my Button $btn = Button->new();
    $btn->setText("Say 'Hello World'");
    $btn->setOnAction( Action->new( sub {
        say "Hello World!";
    } ) );

    my StackPane $root = StackPane->new();
    $root->getChildren()->add($btn);

    my Scene $scene = Scene->new( $root, 300, 250 );

    $primaryStage->setTitle("Hello World!");
    $primaryStage->setScene($scene);
    $primaryStage->show();

    return;
}




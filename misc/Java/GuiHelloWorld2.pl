 
# Example GUI application using JavaFX
#
# $ perl perlito5.pl -Isrc5/lib -Cjava misc/Java/GuiHelloWorld2.pl > MainGui.java
# $ javac MainGui.java
# $ java MainGui
#

package  Application  { import => "javafx.application.Application" }
package  ActionEvent  { import => "javafx.event.ActionEvent" }
package  Scene        { import => "javafx.scene.Scene" }
package  Button       { import => "javafx.scene.control.Button" }
package  StackPane    { import => "javafx.scene.layout.StackPane" }
package  Stage        { import => "javafx.stage.Stage" }
package  EventHandler::ActionEvent {
    import    => "javafx.event.EventHandler",
    java_type => "EventHandler<ActionEvent>",
}
 
package MainGui {
    extends => 'Application',
    decl => [ "public" ],
    'Java::inline' => "
        public static void main(String[] args) {
            Main.init();
            launch(args);
        }
    ",
    methods => [
        start => {
            'Java::inline' => " \@Override \n",
            decl => [ "public" ],               # public method
            args => [ "Stage" ],                # 1 argument of type 'Stage'
            return => "void",                   # return void
            code => "main::action",             # implemented in Perl, see below
        },
    ]
}

package Action {
    implements => "EventHandler::ActionEvent",
    'Java::inline' => "
        PlClosure code;
        public Action(PlClosure code) {
            this.code = code;
        }
        public void handle(ActionEvent action) {
            this.code.apply(PlCx.VOID, new PlArray( new pActionEvent(action) ));
        }
    ",
}

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




use strict;
use warnings;

package header { java_path => 'org.perlito.perlitosample' };

package Android::Activity { import => 'android.support.v7.app.AppCompatActivity' };

package Android::Bundle { import => 'android.os.Bundle' };

package String {};

package Button   { import => 'android.widget.Button' };
package TextView { import => 'android.widget.TextView' };
package EditText { import => 'android.widget.EditText' };
package View     { import => 'android.view.View' };



package MainActivity {
    extends => 'Android::Activity',
    decl    => ['public'],
    'Java::inline' => '
        static {
            Main.init();
        }
        public void onCreate(Bundle saved) {
            super.onCreate(saved);
            setContentView(R.layout.activity_main);
            Button button = (Button) findViewById(R.id.button_ok);
            button.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                  EditText txt = (EditText) findViewById(R.id.value);
                  String x = txt.getText().toString();

                  TextView tv = (TextView) findViewById(R.id.text);
                  String val = doit(x);

                  tv.setText(val);
                }
          });
        }
    ',
    methods => [
        doit => {
            decl   => ['public'],
            return => 'String',
            args   => ['String'],
            code   => 'main::doit',
        }
    ],
}

package main;

sub doit {
    # my ($this, $bundle) = @_;
    my $this = shift;
    my $x = shift;
    if ($x eq 'die') { die "Death to you"; }
    if (0+$x eq $x) {
        return "The result (calculated in Perl) is ". (2 * $x);
    }
    else {
        return "The result (calculated in Perl) is ". ($x x 2);
    }
}




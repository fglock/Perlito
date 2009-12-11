# Do not edit this file - Generated by MiniPerl6
use v5;
use strict;
use MiniPerl6::Perl5::Runtime;
use MiniPerl6::Perl5::Match;
package MiniPerl6::Go::LexicalBlock;
sub new { shift; bless { @_ }, "MiniPerl6::Go::LexicalBlock" }
sub block { @_ == 1 ? ( $_[0]->{block} ) : ( $_[0]->{block} = $_[1] ) };
sub needs_return { @_ == 1 ? ( $_[0]->{needs_return} ) : ( $_[0]->{needs_return} = $_[1] ) };
sub top_level { @_ == 1 ? ( $_[0]->{top_level} ) : ( $_[0]->{top_level} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; do { if (@{$self->{block}}) {  } else { return('') } }; (my  $str = ''); do { for my $decl ( @{$self->{block}} ) { do { if ((Main::isa($decl, 'Decl') && ($decl->decl() eq 'my'))) { ($str = ($str . ($decl->var()->emit_go() . (' := new(Scalar);' . Main->newline()))));do { if (($decl->var()->sigil() eq '%')) { ($str = ($str . ($decl->var()->emit_go() . ('.Bind( h_hash() );' . Main->newline())))) } else { do { if (($decl->var()->sigil() eq '@')) { ($str = ($str . ($decl->var()->emit_go() . ('.Bind( a_array() );' . Main->newline())))) } else { ($str = ($str . ($decl->var()->emit_go() . ('.Bind( u_undef );' . Main->newline())))) } } } } } else {  } };do { if ((Main::isa($decl, 'Bind') && (Main::isa($decl->parameters(), 'Decl') && ($decl->parameters()->decl() eq 'my')))) { ($str = ($str . ($decl->parameters()->var()->emit_go() . ' := new(Scalar);'))) } else {  } } } }; my  $last_statement; do { if ($self->{needs_return}) { ($last_statement = pop( @{$self->{block}} )) } else {  } }; do { for my $decl ( @{$self->{block}} ) { do { if ((Main::isa($decl, 'Decl') && ($decl->decl() eq 'my'))) {  } else { ($str = ($str . ($decl->emit_go() . ';'))) } } } }; do { if (($self->{needs_return} && $last_statement)) { do { if (Main::isa($last_statement, 'If')) { (my  $cond = $last_statement->cond());(my  $body = $last_statement->body());(my  $otherwise = $last_statement->otherwise());do { if ((Main::isa($cond, 'Apply') && ($cond->code() eq 'prefix:<!>'))) { ($cond = $cond->arguments()->[0]);($body = $last_statement->otherwise());($otherwise = $last_statement->body()) } else {  } };do { if ((Main::isa($cond, 'Var') && ($cond->sigil() eq '@'))) { ($cond = Apply->new( 'code' => 'prefix:<@>','arguments' => [$cond], )) } else {  } };($body = MiniPerl6::Go::LexicalBlock->new( 'block' => $body,'needs_return' => 1,'top_level' => $self->{top_level}, ));($otherwise = MiniPerl6::Go::LexicalBlock->new( 'block' => $otherwise,'needs_return' => 1,'top_level' => $self->{top_level}, ));($str = ($str . ('if ( (' . ($cond->emit_go() . (').Bool().b ) { ' . ($body->emit_go() . (' } else { ' . ($otherwise->emit_go() . ' }')))))))) } else { do { if ((Main::isa($last_statement, 'Return') || Main::isa($last_statement, 'For'))) { ($str = ($str . $last_statement->emit_go())) } else { do { if ($self->{top_level}) { ($str = ($str . ('Go_return( p, ' . ($last_statement->emit_go() . ')')))) } else { ($str = ($str . ('return(' . ($last_statement->emit_go() . ')')))) } } } } } } } else {  } }; return($str) }


;
package CompUnit;
sub new { shift; bless { @_ }, "CompUnit" }
sub name { @_ == 1 ? ( $_[0]->{name} ) : ( $_[0]->{name} = $_[1] ) };
sub attributes { @_ == 1 ? ( $_[0]->{attributes} ) : ( $_[0]->{attributes} = $_[1] ) };
sub methods { @_ == 1 ? ( $_[0]->{methods} ) : ( $_[0]->{methods} = $_[1] ) };
sub body { @_ == 1 ? ( $_[0]->{body} ) : ( $_[0]->{body} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; (my  $class_name = Main::to_go_namespace($self->{name})); (my  $str = ('// instances of class ' . ($self->{name} . (Main->newline() . ('type ' . ($class_name . (' struct {' . Main->newline()))))))); do { for my $decl ( @{$self->{body}} ) { do { if ((Main::isa($decl, 'Decl') && ($decl->decl() eq 'has'))) { ($str = ($str . ('  ' . ('v_' . ($decl->var()->name() . (' Scalar;' . Main->newline())))))) } else {  } } } }; ($str = ($str . ('}' . Main->newline()))); ($str = ($str . ('// methods in class ' . ($self->{name} . (Main->newline() . ('var Method_' . ($class_name . (' struct {' . Main->newline())))))))); do { for my $decl ( @{$self->{body}} ) { do { if (Main::isa($decl, 'Method')) { ($str = ($str . ('  ' . ('f_' . ($decl->name() . (' func (*' . ($class_name . (', Capture) Any;' . Main->newline())))))))) } else {  } };do { if ((Main::isa($decl, 'Decl') && ($decl->decl() eq 'has'))) { ($str = ($str . ('  ' . ('f_' . ($decl->var()->name() . (' func (*' . ($class_name . (', Capture) Any;' . Main->newline())))))))) } else {  } } } }; ($str = ($str . ('}' . Main->newline()))); ($str = ($str . ('// namespace ' . ($self->{name} . (Main->newline() . ('var Namespace_' . ($class_name . (' struct {' . Main->newline())))))))); do { for my $decl ( @{$self->{body}} ) { do { if (Main::isa($decl, 'Sub')) { ($str = ($str . ('  ' . ('f_' . ($decl->name() . (' Function;' . Main->newline())))))) } else {  } } } }; ($str = ($str . ('}' . Main->newline()))); ($str = ($str . ('var Run_' . ($class_name . (' func ();' . Main->newline()))))); ($str = ($str . ('// method wrappers for ' . ($self->{name} . Main->newline())))); do { for my $decl ( @{$self->{body}} ) { do { if (Main::isa($decl, 'Method')) { ($str = ($str . ('func (v_self *' . ($class_name . (') f_' . ($decl->name() . (' (v Capture) Any {' . (Main->newline() . ('  return Method_' . ($class_name . ('.f_' . ($decl->name() . ('(v_self, v);' . (Main->newline() . ('}' . Main->newline()))))))))))))))) } else {  } };do { if ((Main::isa($decl, 'Decl') && ($decl->decl() eq 'has'))) { ($str = ($str . ('func (v_self *' . ($class_name . (') f_' . ($decl->var()->name() . (' (v Capture) Any {' . (Main->newline() . ('  return Method_' . ($class_name . ('.f_' . ($decl->var()->name() . ('(v_self, v);' . (Main->newline() . ('}' . Main->newline()))))))))))))))) } else {  } } } }; ($str = ($str . ('func (v_self ' . ($class_name . (') Bool () Bool { return b_true }' . Main->newline()))))); ($str = ($str . ('func (v_self ' . ($class_name . (') Int () Int { panic("converting class to int") }' . Main->newline()))))); ($str = ($str . ('func (v_self ' . ($class_name . (') Str () Str { panic("converting class to string") }' . Main->newline()))))); ($str = ($str . ('func (v_self ' . ($class_name . (') Array () Array { panic("converting class to array") }' . Main->newline()))))); ($str = ($str . ('func (v_self ' . ($class_name . (') Hash () Hash { panic("converting class to hash") }' . Main->newline()))))); ($str = ($str . ('func (v_self ' . ($class_name . (') Equal (j Any) Bool { panic("comparing class") }' . Main->newline()))))); ($str = ($str . ('func (v_self ' . ($class_name . (') Fetch () Any { return v_self }' . Main->newline()))))); ($str = ($str . ('func (v_self ' . ($class_name . (') f_isa (v Capture) Any { ' . ('return Str{ s : "' . ($class_name . ('" }.Str_equal( v.p[0] ) ' . ('}' . Main->newline()))))))))); ($str = ($str . ('// prototype of ' . ($self->{name} . (Main->newline() . ('var Proto_' . ($class_name . (' Scalar;' . Main->newline())))))))); ($str = ($str . ('func Init_' . ($class_name . ('() {' . Main->newline()))))); ($str = ($str . ('  this_namespace := &Namespace_' . ($class_name . (';' . (Main->newline() . ('  this_namespace = this_namespace;' . (Main->newline() . ('  Proto_' . ($class_name . ('.Bind( new(' . ($class_name . (') );' . Main->newline()))))))))))))); do { for my $decl ( @{$self->{body}} ) { do { if ((Main::isa($decl, 'Decl') && ($decl->decl() eq 'my'))) { ($str = ($str . ('  ' . ($decl->var()->emit_go() . (' := new(Scalar);' . Main->newline())))));do { if (($decl->var()->sigil() eq '%')) { ($str = ($str . ($decl->var()->emit_go() . ('.Bind( h_hash() );' . Main->newline())))) } else { do { if (($decl->var()->sigil() eq '@')) { ($str = ($str . ($decl->var()->emit_go() . ('.Bind( a_array() );' . Main->newline())))) } else { ($str = ($str . ($decl->var()->emit_go() . ('.Bind( u_undef );' . Main->newline())))) } } } };($str = ($str . Main->newline())) } else {  } };do { if ((Main::isa($decl, 'Bind') && (Main::isa($decl->parameters(), 'Decl') && ($decl->parameters()->decl() eq 'my')))) { ($str = ($str . ('  ' . ($decl->parameters()->var()->emit_go() . (' := new(Scalar);' . Main->newline()))))) } else {  } } } }; do { for my $decl ( @{$self->{body}} ) { do { if ((Main::isa($decl, 'Decl') && ($decl->decl() eq 'has'))) { ($str = ($str . ('  // accessor ' . ($decl->var()->name() . (Main->newline() . ('  Method_' . ($class_name . ('.f_' . ($decl->var()->name() . (' = func (v_self *' . ($class_name . (', v Capture) Any {' . (Main->newline() . ('    ' . ('return v_self.v_' . ($decl->var()->name() . (Main->newline() . ('  };' . Main->newline())))))))))))))))))) } else {  } };do { if (Main::isa($decl, 'Method')) { (my  $sig = $decl->sig());(my  $block = MiniPerl6::Go::LexicalBlock->new( 'block' => $decl->block(),'needs_return' => 1,'top_level' => 1, ));($str = ($str . ('  // method ' . ($decl->name() . (Main->newline() . ('  Method_' . ($class_name . ('.f_' . ($decl->name() . (' = func (' . ($sig->invocant()->emit_go() . (' *' . ($class_name . (', v Capture) Any {' . (Main->newline() . ('    ' . ($sig->emit_go_bind() . (Main->newline() . ('    p := make(chan Any); go func () { ' . (Main->newline() . ('        ' . ($block->emit_go() . ('; return }(); ' . (Main->newline() . ('    return <-p; ' . (Main->newline() . ('  };' . Main->newline()))))))))))))))))))))))))))) } else {  } };do { if (Main::isa($decl, 'Sub')) { (my  $sig = $decl->sig());(my  $block = MiniPerl6::Go::LexicalBlock->new( 'block' => $decl->block(),'needs_return' => 1,'top_level' => 1, ));($str = ($str . ('  // sub ' . ($decl->name() . (Main->newline() . ('  Namespace_' . ($class_name . ('.f_' . ($decl->name() . (' = Function{ f : func (v Capture) Any {' . (Main->newline() . ('    ' . ($sig->emit_go_bind() . (Main->newline() . ('    p := make(chan Any); go func () { ' . (Main->newline() . ('        ' . ($block->emit_go() . ('; return }(); ' . (Main->newline() . ('    return <-p; ' . (Main->newline() . ('  } };' . Main->newline()))))))))))))))))))))))) } else {  } } } }; ($str = ($str . ('  // main runtime block of ' . ($self->{name} . (Main->newline() . ('  Run_' . ($class_name . (' = func () {' . Main->newline())))))))); do { for my $decl ( @{$self->{body}} ) { do { if ((((Main::isa($decl, 'Decl') && (($decl->decl() eq 'has') || ($decl->decl() eq 'my'))) ? 0 : 1) && ((Main::isa($decl, 'Method') ? 0 : 1) && (Main::isa($decl, 'Sub') ? 0 : 1)))) { ($str = ($str . ('    ' . ($decl->emit_go() . (';' . Main->newline()))))) } else {  } } } }; ($str = ($str . ('  }' . Main->newline()))); ($str = ($str . ('}' . Main->newline()))); return($str) }


;
package Val::Int;
sub new { shift; bless { @_ }, "Val::Int" }
sub int { @_ == 1 ? ( $_[0]->{int} ) : ( $_[0]->{int} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; ('Int{i:' . ($self->{int} . '}')) }


;
package Val::Bit;
sub new { shift; bless { @_ }, "Val::Bit" }
sub bit { @_ == 1 ? ( $_[0]->{bit} ) : ( $_[0]->{bit} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; ('Bit{b:' . ($self->{bit} . '}')) }


;
package Val::Num;
sub new { shift; bless { @_ }, "Val::Num" }
sub num { @_ == 1 ? ( $_[0]->{num} ) : ( $_[0]->{num} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; ('Num{n:' . ($self->{num} . '}')) }


;
package Val::Buf;
sub new { shift; bless { @_ }, "Val::Buf" }
sub buf { @_ == 1 ? ( $_[0]->{buf} ) : ( $_[0]->{buf} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; ('Str{s:"' . (Main::javascript_escape_string($self->{buf}) . '"}')) }


;
package Val::Undef;
sub new { shift; bless { @_ }, "Val::Undef" }
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; 'u_undef' }


;
package Val::Object;
sub new { shift; bless { @_ }, "Val::Object" }
sub class { @_ == 1 ? ( $_[0]->{class} ) : ( $_[0]->{class} = $_[1] ) };
sub fields { @_ == 1 ? ( $_[0]->{fields} ) : ( $_[0]->{fields} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; die('Val::Object - not used yet') }


;
package Lit::Seq;
sub new { shift; bless { @_ }, "Lit::Seq" }
sub seq { @_ == 1 ? ( $_[0]->{seq} ) : ( $_[0]->{seq} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; ('[]Any{ ' . (Main::join([ map { $_->emit_go() } @{ $self->{seq} } ], ', ') . ' }')) }


;
package Lit::Array;
sub new { shift; bless { @_ }, "Lit::Array" }
sub array { @_ == 1 ? ( $_[0]->{array} ) : ( $_[0]->{array} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; (my  $str = ''); do { for my $item ( @{$self->{array}} ) { do { if (((Main::isa($item, 'Var') && ($item->sigil() eq '@')) || (Main::isa($item, 'Apply') && ($item->code() eq 'prefix:<@>')))) { ($str = ($str . ('func(a_ Array) { ' . ('for i_ := 0; i_ < a_.n; i_++ { a.Push(a_.v[i_]) }' . ('}( ' . ($item->emit_go() . '.Fetch().Array() ); ')))))) } else { ($str = ($str . ('a.Push(' . ($item->emit_go() . '); ')))) } } } }; ('func () Array { ' . ('a := a_array(); ' . ($str . ('return a; ' . '}()')))) }


;
package Lit::Hash;
sub new { shift; bless { @_ }, "Lit::Hash" }
sub hash { @_ == 1 ? ( $_[0]->{hash} ) : ( $_[0]->{hash} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; (my  $fields = $self->{hash}); (my  $str = ''); do { for my $field ( @{$fields} ) { ($str = ($str . ('m.Lookup( ' . ($field->[0]->emit_go() . (' ).Bind( ' . ($field->[1]->emit_go() . ' ); ')))))) } }; ('func() Hash { ' . ('m := h_hash(); ' . ($str . ('return m; ' . '}()')))) }


;
package Lit::Code;
sub new { shift; bless { @_ }, "Lit::Code" }
1


;
package Lit::Object;
sub new { shift; bless { @_ }, "Lit::Object" }
sub class { @_ == 1 ? ( $_[0]->{class} ) : ( $_[0]->{class} = $_[1] ) };
sub fields { @_ == 1 ? ( $_[0]->{fields} ) : ( $_[0]->{fields} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; (my  $fields = $self->{fields}); (my  $str = ''); do { for my $field ( @{$fields} ) { ($str = ($str . ('m.v_' . ($field->[0]->buf() . ('.Bind( ' . ($field->[1]->emit_go() . ' ); ')))))) } }; ('func() *' . (Main::to_go_namespace($self->{class}) . (' { ' . ('var m = new(' . (Main::to_go_namespace($self->{class}) . ('); ' . ($str . ('return m; ' . '}()')))))))) }


;
package Index;
sub new { shift; bless { @_ }, "Index" }
sub obj { @_ == 1 ? ( $_[0]->{obj} ) : ( $_[0]->{obj} = $_[1] ) };
sub index_exp { @_ == 1 ? ( $_[0]->{index_exp} ) : ( $_[0]->{index_exp} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; ($self->{obj}->emit_go() . ('.Array().Index(' . ($self->{index_exp}->emit_go() . ')'))) }


;
package Lookup;
sub new { shift; bless { @_ }, "Lookup" }
sub obj { @_ == 1 ? ( $_[0]->{obj} ) : ( $_[0]->{obj} = $_[1] ) };
sub index_exp { @_ == 1 ? ( $_[0]->{index_exp} ) : ( $_[0]->{index_exp} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; ($self->{obj}->emit_go() . ('.Hash().Lookup(' . ($self->{index_exp}->emit_go() . ')'))) }


;
package Var;
sub new { shift; bless { @_ }, "Var" }
sub sigil { @_ == 1 ? ( $_[0]->{sigil} ) : ( $_[0]->{sigil} = $_[1] ) };
sub twigil { @_ == 1 ? ( $_[0]->{twigil} ) : ( $_[0]->{twigil} = $_[1] ) };
sub namespace { @_ == 1 ? ( $_[0]->{namespace} ) : ( $_[0]->{namespace} = $_[1] ) };
sub name { @_ == 1 ? ( $_[0]->{name} ) : ( $_[0]->{name} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; (my  $table = { '$' => 'v_','@' => 'List_','%' => 'Hash_','&' => 'Code_', }); (my  $ns = ''); do { if ($self->{namespace}) { ($ns = (Main::to_go_namespace($self->{namespace}) . '.')) } else {  } }; (($self->{twigil} eq '.') ? ('v_self.v_' . ($self->{name} . '')) : (($self->{name} eq '/') ? 'Proto_MiniPerl6__Match' : ($table->{$self->{sigil}} . ($ns . $self->{name})))) };
sub plain_name { my $self = shift; my $List__ = \@_; do { [] }; do { if ($self->{namespace}) { return(($self->{namespace} . ('.' . $self->{name}))) } else {  } }; return($self->{name}) }


;
package Bind;
sub new { shift; bless { @_ }, "Bind" }
sub parameters { @_ == 1 ? ( $_[0]->{parameters} ) : ( $_[0]->{parameters} = $_[1] ) };
sub arguments { @_ == 1 ? ( $_[0]->{arguments} ) : ( $_[0]->{arguments} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; do { if (Main::isa($self->{parameters}, 'Lit::Array')) { (my  $a = $self->{parameters}->array());(my  $str = ('func () Any { ' . ('List_tmp := ' . ($self->{arguments}->emit_go() . '; '))));(my  $i = 0);do { for my $var ( @{$a} ) { (my  $bind = Bind->new( 'parameters' => $var,'arguments' => Index->new( 'obj' => Var->new( 'sigil' => '@','twigil' => '','namespace' => '','name' => 'tmp', ),'index_exp' => Val::Int->new( 'int' => $i, ), ), ));($str = ($str . (' ' . ($bind->emit_go() . '; '))));($i = ($i + 1)) } };return(($str . ' return List_tmp }()')) } else {  } }; do { if (Main::isa($self->{parameters}, 'Lit::Hash')) { (my  $a = $self->{parameters}->hash());(my  $b = $self->{arguments}->hash());(my  $str = 'do { ');(my  $i = 0);my  $arg;do { for my $var ( @{$a} ) { ($arg = Val::Undef->new(  ));do { for my $var2 ( @{$b} ) { do { if (($var2->[0]->buf() eq $var->[0]->buf())) { ($arg = $var2->[1]) } else {  } } } };(my  $bind = Bind->new( 'parameters' => $var->[1],'arguments' => $arg, ));($str = ($str . (' ' . ($bind->emit_go() . '; '))));($i = ($i + 1)) } };return(($str . ($self->{parameters}->emit_go() . ' }'))) } else {  } }; do { if (Main::isa($self->{parameters}, 'Lit::Object')) { (my  $class = $self->{parameters}->class());(my  $a = $self->{parameters}->fields());(my  $b = $self->{arguments});(my  $str = 'do { ');(my  $i = 0);my  $arg;do { for my $var ( @{$a} ) { (my  $bind = Bind->new( 'parameters' => $var->[1],'arguments' => Call->new( 'invocant' => $b,'method' => $var->[0]->buf(),'arguments' => [],'hyper' => 0, ), ));($str = ($str . (' ' . ($bind->emit_go() . '; '))));($i = ($i + 1)) } };return(($str . ($self->{parameters}->emit_go() . ' }'))) } else {  } }; do { if (Main::isa($self->{parameters}, 'Call')) { return(('func () Any { ' . ('var tmp Scalar = ' . ($self->{parameters}->invocant()->emit_go() . ('.Fetch().(' . ($self->{parameters}->method() . ('_er)' . ('.f_' . ($self->{parameters}->method() . ('( Capture{ p : []Any{  } } ).(Scalar); ' . ('tmp.Bind( ' . ($self->{arguments}->emit_go() . (' ); ' . ('return tmp; ' . '}()')))))))))))))) } else {  } }; ($self->{parameters}->emit_go() . ('.Bind( ' . ($self->{arguments}->emit_go() . ' )'))) }


;
package Proto;
sub new { shift; bless { @_ }, "Proto" }
sub name { @_ == 1 ? ( $_[0]->{name} ) : ( $_[0]->{name} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; Main::to_go_namespace($self->{name}) }


;
package Call;
sub new { shift; bless { @_ }, "Call" }
sub invocant { @_ == 1 ? ( $_[0]->{invocant} ) : ( $_[0]->{invocant} = $_[1] ) };
sub hyper { @_ == 1 ? ( $_[0]->{hyper} ) : ( $_[0]->{hyper} = $_[1] ) };
sub method { @_ == 1 ? ( $_[0]->{method} ) : ( $_[0]->{method} = $_[1] ) };
sub arguments { @_ == 1 ? ( $_[0]->{arguments} ) : ( $_[0]->{arguments} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; (my  $invocant = $self->{invocant}->emit_go()); do { if (Main::isa($self->{invocant}, 'Proto')) { ($invocant = ('Proto_' . $invocant)) } else {  } }; do { if ((($self->{method} eq 'yaml') || (($self->{method} eq 'say') || ($self->{method} eq 'chars')))) { do { if ($self->{hyper}) { return(('func (a_ Any) Array { ' . ('var out = a_array(); ' . ('if ( typeof a_ == \'undefined\' ) { return out }; ' . ('for(var i = 0; i < a_.length; i++) { ' . ('out.Push( Main.' . ($self->{method} . ('(a_[i]) ) } return out;' . (' }(' . ($invocant . ')')))))))))) } else { do { if (defined($self->{arguments})) { return(('Main.' . ($self->{method} . ('(' . ($invocant . (', ' . (Main::join([ map { $_->emit_go() } @{ $self->{arguments} } ], ', ') . ')'))))))) } else { return(('Main.' . ($self->{method} . ('(' . ($invocant . ')'))))) } } } } } else {  } }; (my  $meth = $self->{method}); do { if (($meth eq 'postcircumfix:<( )>')) { do { if ($self->{hyper}) { ($meth = '') } else { return(($invocant . ('.Apply( Capture{ p : []Any{ ' . (Main::join([ map { $_->emit_go() } @{ $self->{arguments} } ], ', ') . ' } } )')))) } } } else {  } }; do { if ($self->{hyper}) { ('func (a_ Any) Array { ' . ('var out = a_array(); ' . ('var i Array = a_.Array(); ' . ('for pos := 0; pos < i.n; pos++ { ' . ('out.Push( i.v[pos].Fetch().(' . ($meth . ('_er).f_' . ($meth . ('(Capture{ p : []Any{}  })) } ' . ('return out; ' . ('}(' . ($invocant . ')')))))))))))) } else { ($invocant . ('.Fetch().(' . ($meth . ('_er).f_' . ($meth . ('( Capture{ p : []Any{ ' . (Main::join([ map { $_->emit_go() } @{ $self->{arguments} } ], ', ') . ' } } )'))))))) } } }


;
package Apply;
sub new { shift; bless { @_ }, "Apply" }
sub code { @_ == 1 ? ( $_[0]->{code} ) : ( $_[0]->{code} = $_[1] ) };
sub arguments { @_ == 1 ? ( $_[0]->{arguments} ) : ( $_[0]->{arguments} = $_[1] ) };
sub namespace { @_ == 1 ? ( $_[0]->{namespace} ) : ( $_[0]->{namespace} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; (my  $code = $self->{code}); do { if (Main::isa($code, 'Str')) {  } else { return(('(' . ($self->{code}->emit_go() . (')->(' . (Main::join([ map { $_->emit() } @{ $self->{arguments} } ], ', ') . ')'))))) } }; do { if (($code eq 'self')) { return('v_self') } else {  } }; do { if (($code eq 'false')) { return('b_false') } else {  } }; do { if (($code eq 'make')) { return(('func () Any { ' . ('tmp := ' . (Main::join([ map { $_->emit_go() } @{ $self->{arguments} } ], ', ') . ('; ' . ('Proto_MiniPerl6__Match.v_capture = tmp; ' . ('return tmp; ' . '}()'))))))) } else {  } }; do { if (($code eq 'say')) { return(('Print( Capture{ p : []Any{ ' . (Main::join([ map { $_->emit_go() } @{ $self->{arguments} } ], ', ') . ', Str{s:"\\n"} } } )'))) } else {  } }; do { if (($code eq 'print')) { return(('Print( Capture{ p : []Any{ ' . (Main::join([ map { $_->emit_go() } @{ $self->{arguments} } ], ', ') . ' } } )'))) } else {  } }; do { if (($code eq 'warn')) { return(('Print_stderr( Capture{ p : []Any{ ' . (Main::join([ map { $_->emit_go() } @{ $self->{arguments} } ], ', ') . ', Str{s:"\\n"} } } )'))) } else {  } }; do { if (($code eq 'die')) { return(('Die( Capture{ p : []Any{ ' . (Main::join([ map { $_->emit_go() } @{ $self->{arguments} } ], ', ') . ' } } )'))) } else {  } }; do { if (($code eq 'defined')) { return((Main::join([ map { $_->emit_go() } @{ $self->{arguments} } ], ' ') . '.Defined()')) } else {  } }; do { if (($code eq 'pop')) { return(('Pop(' . (Main::join([ map { $_->emit_go() } @{ $self->{arguments} } ], ' ') . ')'))) } else {  } }; do { if (($code eq 'substr')) { return(('Substr( Capture{ p : []Any{ ' . (Main::join([ map { $_->emit_go() } @{ $self->{arguments} } ], ', ') . ' } } )'))) } else {  } }; do { if (($code eq 'prefix:<~>')) { return(('(' . (Main::join([ map { $_->emit_go() } @{ $self->{arguments} } ], ', ') . ').Str()'))) } else {  } }; do { if (($code eq 'prefix:<!>')) { return(('(' . (Main::join([ map { $_->emit_go() } @{ $self->{arguments} } ], ', ') . ').Bool().Not()'))) } else {  } }; do { if (($code eq 'prefix:<?>')) { return(('(' . (Main::join([ map { $_->emit_go() } @{ $self->{arguments} } ], ', ') . ').Bool()'))) } else {  } }; do { if (($code eq 'prefix:<$>')) { return(('f_scalar( Capture{ p : []Any{ ' . (Main::join([ map { $_->emit_go() } @{ $self->{arguments} } ], ', ') . ' } } )'))) } else {  } }; do { if (($code eq 'prefix:<@>')) { return(('(' . (Main::join([ map { $_->emit_go() } @{ $self->{arguments} } ], ', ') . ')'))) } else {  } }; do { if (($code eq 'prefix:<%>')) { return(('(' . (Main::join([ map { $_->emit_go() } @{ $self->{arguments} } ], ', ') . ').f_hash()'))) } else {  } }; do { if (($code eq 'infix:<~>')) { return(('Str{ s: strings.Join( []string{' . ('(' . ($self->{arguments}->[0]->emit_go() . (').Str().s, ' . ('(' . ($self->{arguments}->[1]->emit_go() . (').Str().s' . '}, "" ) }')))))))) } else {  } }; do { if (($code eq 'infix:<+>')) { return(('Int{ i:' . ('(' . ($self->{arguments}->[0]->emit_go() . (').Int().i + ' . ('(' . ($self->{arguments}->[1]->emit_go() . (').Int().i ' . '}')))))))) } else {  } }; do { if (($code eq 'infix:<->')) { return(('Int{ i:' . ('(' . ($self->{arguments}->[0]->emit_go() . (').Int().i - ' . ('(' . ($self->{arguments}->[1]->emit_go() . (').Int().i ' . '}')))))))) } else {  } }; do { if (($code eq 'infix:<>>')) { return(('Bool{ b:' . ('(' . ($self->{arguments}->[0]->emit_go() . (').Int().i > ' . ('(' . ($self->{arguments}->[1]->emit_go() . (').Int().i ' . '}')))))))) } else {  } }; do { if (($code eq 'infix:<&&>')) { return(('Bool{ b: (' . ($self->{arguments}->[0]->emit_go() . (').Bool().b' . (' && (' . ($self->{arguments}->[1]->emit_go() . ').Bool().b }')))))) } else {  } }; do { if (($code eq 'infix:<||>')) { return(('Bool{ b: (' . ($self->{arguments}->[0]->emit_go() . (').Bool().b' . (' || (' . ($self->{arguments}->[1]->emit_go() . ').Bool().b }')))))) } else {  } }; do { if (($code eq 'infix:<eq>')) { return(('(' . ($self->{arguments}->[0]->emit_go() . (').Str().Str_equal(' . ($self->{arguments}->[1]->emit_go() . ')'))))) } else {  } }; do { if (($code eq 'infix:<ne>')) { return(('(' . ($self->{arguments}->[0]->emit_go() . (').Str().Str_equal(' . ($self->{arguments}->[1]->emit_go() . ').Not()'))))) } else {  } }; do { if (($code eq 'infix:<==>')) { return(('(' . ($self->{arguments}->[0]->emit_go() . (').Equal(' . ($self->{arguments}->[1]->emit_go() . ')'))))) } else {  } }; do { if (($code eq 'infix:<!=>')) { return(('(' . ($self->{arguments}->[0]->emit_go() . (').Equal(' . ($self->{arguments}->[1]->emit_go() . ').Not()'))))) } else {  } }; do { if (($code eq 'ternary:<?? !!>')) { return(('func () Any { ' . ('if (' . ($self->{arguments}->[0]->emit_go() . (').Bool().b ' . ('{ return ' . ($self->{arguments}->[1]->emit_go() . (' }; ' . ('return ' . ($self->{arguments}->[2]->emit_go() . (' ' . '}()'))))))))))) } else {  } }; ($code = ('f_' . $self->{code})); do { if ($self->{namespace}) { ($code = ('Namespace_' . (Main::to_go_namespace($self->{namespace}) . ('.' . $code)))) } else { ($code = ('this_namespace.' . $code)) } }; ($code . ('.Apply( Capture{ p : []Any{ ' . (Main::join([ map { $_->emit_go() } @{ $self->{arguments} } ], ', ') . ' } } )'))) }


;
package Return;
sub new { shift; bless { @_ }, "Return" }
sub result { @_ == 1 ? ( $_[0]->{result} ) : ( $_[0]->{result} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; return(('Go_return( p, ' . ($self->{result}->emit_go() . ')'))) }


;
package If;
sub new { shift; bless { @_ }, "If" }
sub cond { @_ == 1 ? ( $_[0]->{cond} ) : ( $_[0]->{cond} = $_[1] ) };
sub body { @_ == 1 ? ( $_[0]->{body} ) : ( $_[0]->{body} = $_[1] ) };
sub otherwise { @_ == 1 ? ( $_[0]->{otherwise} ) : ( $_[0]->{otherwise} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; (my  $cond = $self->{cond}); do { if ((Main::isa($cond, 'Apply') && ($cond->code() eq 'prefix:<!>'))) { (my  $if = If->new( 'cond' => $cond->arguments()->[0],'body' => $self->{otherwise},'otherwise' => $self->{body}, ));return($if->emit_go()) } else {  } }; do { if ((Main::isa($cond, 'Var') && ($cond->sigil() eq '@'))) { ($cond = Apply->new( 'code' => 'prefix:<@>','arguments' => [$cond], )) } else {  } }; (my  $s = ('if (' . ($cond->emit_go() . (').Bool().b { ' . (MiniPerl6::Go::LexicalBlock->new( 'block' => $self->{body},'needs_return' => 0, )->emit_go() . ' }'))))); do { if (@{$self->{otherwise}}) {  } else { return($s) } }; return(($s . (' else { ' . (MiniPerl6::Go::LexicalBlock->new( 'block' => $self->{otherwise},'needs_return' => 0, )->emit_go() . ' }')))) }


;
package For;
sub new { shift; bless { @_ }, "For" }
sub cond { @_ == 1 ? ( $_[0]->{cond} ) : ( $_[0]->{cond} = $_[1] ) };
sub body { @_ == 1 ? ( $_[0]->{body} ) : ( $_[0]->{body} = $_[1] ) };
sub topic { @_ == 1 ? ( $_[0]->{topic} ) : ( $_[0]->{topic} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; ('func (a_ Any) { ' . ('var i Array = a_.Array(); ' . ('for pos := 0; pos < i.n; pos++ { ' . ('func (' . ($self->{topic}->emit_go() . (' Any) { ' . (Main::join([ map { $_->emit_go() } @{ $self->{body} } ], ';') . (' }(i.v[pos]) ' . ('} ' . ('}(' . ($self->{cond}->emit_go() . ')'))))))))))) }


;
package Decl;
sub new { shift; bless { @_ }, "Decl" }
sub decl { @_ == 1 ? ( $_[0]->{decl} ) : ( $_[0]->{decl} = $_[1] ) };
sub type { @_ == 1 ? ( $_[0]->{type} ) : ( $_[0]->{type} = $_[1] ) };
sub var { @_ == 1 ? ( $_[0]->{var} ) : ( $_[0]->{var} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; $self->{var}->emit_go() }


;
package Sig;
sub new { shift; bless { @_ }, "Sig" }
sub invocant { @_ == 1 ? ( $_[0]->{invocant} ) : ( $_[0]->{invocant} = $_[1] ) };
sub positional { @_ == 1 ? ( $_[0]->{positional} ) : ( $_[0]->{positional} = $_[1] ) };
sub named { @_ == 1 ? ( $_[0]->{named} ) : ( $_[0]->{named} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; ' print \'Signature - TODO\'; die \'Signature - TODO\'; ' };
sub emit_go_bind { my $self = shift; my $List__ = \@_; do { [] }; (my  $str = ''); (my  $i = 0); do { for my $decl ( @{$self->{positional}} ) { ($str = ($str . ($decl->emit_go() . (' := v.p[' . ($i . ']; ')))));($i = ($i + 1)) } }; return($str) }


;
package Method;
sub new { shift; bless { @_ }, "Method" }
sub name { @_ == 1 ? ( $_[0]->{name} ) : ( $_[0]->{name} = $_[1] ) };
sub sig { @_ == 1 ? ( $_[0]->{sig} ) : ( $_[0]->{sig} = $_[1] ) };
sub block { @_ == 1 ? ( $_[0]->{block} ) : ( $_[0]->{block} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; (my  $invocant = $self->{sig}->invocant()); ('func ' . ($self->{name} . ('(v Capture) Any { ' . ('    ' . ($self->{sig}->emit_go_bind() . (Main->newline() . ('    p := make(chan Any); go func () { ' . (Main->newline() . ('        ' . (MiniPerl6::Go::LexicalBlock->new( 'block' => $self->{block},'needs_return' => 1,'top_level' => 1, )->emit_go() . ('; return }(); ' . (Main->newline() . ('    return <-p; ' . (Main->newline() . ' }')))))))))))))) }


;
package Sub;
sub new { shift; bless { @_ }, "Sub" }
sub name { @_ == 1 ? ( $_[0]->{name} ) : ( $_[0]->{name} = $_[1] ) };
sub sig { @_ == 1 ? ( $_[0]->{sig} ) : ( $_[0]->{sig} = $_[1] ) };
sub block { @_ == 1 ? ( $_[0]->{block} ) : ( $_[0]->{block} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; do { if (($self->{name} eq '')) { return(('Function{ f: func(v Capture) Any { ' . ('    ' . ($self->{sig}->emit_go_bind() . (Main->newline() . ('    p := make(chan Any); go func () { ' . (Main->newline() . ('        ' . (MiniPerl6::Go::LexicalBlock->new( 'block' => $self->{block},'needs_return' => 1,'top_level' => 1, )->emit_go() . ('; return }(); ' . (Main->newline() . ('    return <-p; ' . (Main->newline() . ('} ' . '}')))))))))))))) } else {  } }; ('func ' . ($self->{name} . ('(v Capture) Any { ' . ('    ' . ($self->{sig}->emit_go_bind() . (Main->newline() . ('    p := make(chan Any); go func () { ' . (Main->newline() . ('        ' . (MiniPerl6::Go::LexicalBlock->new( 'block' => $self->{block},'needs_return' => 1,'top_level' => 1, )->emit_go() . ('; return }(); ' . (Main->newline() . ('    return <-p; ' . (Main->newline() . ' }')))))))))))))) }


;
package Do;
sub new { shift; bless { @_ }, "Do" }
sub block { @_ == 1 ? ( $_[0]->{block} ) : ( $_[0]->{block} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; ('(func () Any { ' . (MiniPerl6::Go::LexicalBlock->new( 'block' => $self->{block},'needs_return' => 1, )->emit_go() . ('; return u_undef ' . '})()'))) }


;
package Use;
sub new { shift; bless { @_ }, "Use" }
sub mod { @_ == 1 ? ( $_[0]->{mod} ) : ( $_[0]->{mod} = $_[1] ) };
sub emit_go { my $self = shift; my $List__ = \@_; do { [] }; ('// use ' . ($self->{mod} . Main->newline())) }


;
1;

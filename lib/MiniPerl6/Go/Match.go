// MiniPerl6::Match

// instances of class MiniPerl6::Match
type MiniPerl6__Match struct {
	v_from		Scalar;
	v_to		Scalar;
	v_str		Scalar;
	v_bool		Scalar;
	v_capture	Scalar;
    v_hash      Hash;
    v_array     Array;
}
// methods in class MiniPerl6::Match
var Method_MiniPerl6__Match struct {
	f_from		func(*MiniPerl6__Match, Capture) Any;
	f_to		func(*MiniPerl6__Match, Capture) Any;
	f_str		func(*MiniPerl6__Match, Capture) Any;
	f_bool		func(*MiniPerl6__Match, Capture) Any;
	f_capture	func(*MiniPerl6__Match, Capture) Any;
	f_scalar	func(*MiniPerl6__Match, Capture) Any;
	f_string	func(*MiniPerl6__Match, Capture) Any;
}
// namespace MiniPerl6::Match
var Namespace_MiniPerl6__Match struct{}
var Run_MiniPerl6__Match func()
// method wrappers for MiniPerl6::Match
func (v_self *MiniPerl6__Match) f_from(v Capture) Any {
	return Method_MiniPerl6__Match.f_from(v_self, v)
}
func (v_self *MiniPerl6__Match) f_to(v Capture) Any {
	return Method_MiniPerl6__Match.f_to(v_self, v)
}
func (v_self *MiniPerl6__Match) f_str(v Capture) Any {
	return Method_MiniPerl6__Match.f_str(v_self, v)
}
func (v_self *MiniPerl6__Match) f_bool(v Capture) Any {
	return Method_MiniPerl6__Match.f_bool(v_self, v)
}
func (v_self *MiniPerl6__Match) f_capture(v Capture) Any {
	return Method_MiniPerl6__Match.f_capture(v_self, v)
}
func (v_self *MiniPerl6__Match) f_scalar(v Capture) Any {
	return Method_MiniPerl6__Match.f_scalar(v_self, v)
}
func (v_self *MiniPerl6__Match) f_string(v Capture) Any {
	return Method_MiniPerl6__Match.f_string(v_self, v)
}
func (v_self MiniPerl6__Match) Bool() Bool	{ return b_true }
func (v_self MiniPerl6__Match) Int() Int	{ panic("converting class to int") }
func (v_self MiniPerl6__Match) Str() Str	{ panic("converting class to string") }
func (v_self MiniPerl6__Match) Array() Array	{ return v_self.v_array }
func (v_self MiniPerl6__Match) Hash() Hash	{ return v_self.v_hash }
func (v_self MiniPerl6__Match) Equal(j Any) Bool {
	panic("comparing class")
}
func (v_self MiniPerl6__Match) Fetch() Any	{ return v_self }
// prototype of MiniPerl6::Match
var Proto_MiniPerl6__Match Scalar

func Init_MiniPerl6__Match() {
	this_namespace := &Namespace_MiniPerl6__Match;
	this_namespace = this_namespace;

    Proto_MiniPerl6__Match.Bind(
        func() *MiniPerl6__Match {
            var m = new(MiniPerl6__Match);
            m.v_hash = h_hash();
            return m;
        }() );

	// accessor from
	Method_MiniPerl6__Match.f_from = func(v_self *MiniPerl6__Match, v Capture) Any {
		return v_self.v_from
	};
	// accessor to
	Method_MiniPerl6__Match.f_to = func(v_self *MiniPerl6__Match, v Capture) Any {
		return v_self.v_to
	};
	// accessor str
	Method_MiniPerl6__Match.f_str = func(v_self *MiniPerl6__Match, v Capture) Any {
		return v_self.v_str
	};
	// accessor bool
	Method_MiniPerl6__Match.f_bool = func(v_self *MiniPerl6__Match, v Capture) Any {
		return v_self.v_bool
	};
	// accessor capture
	Method_MiniPerl6__Match.f_capture = func(v_self *MiniPerl6__Match, v Capture) Any {
		return v_self.v_capture
	};
	// method scalar
	Method_MiniPerl6__Match.f_scalar = func(v_self *MiniPerl6__Match, v Capture) Any {

		p := make(chan Any);
		go func() {
			if (v_self.v_bool).Bool().b {
				if v_self.v_capture.Defined().Bool().b {
					Go_return(p, v_self.v_capture)
				} else {
				}
				Go_return(p, Substr(Capture{p: []Any{v_self.v_str, v_self.v_from, Int{i: (v_self.v_to).Int().i - (v_self.v_from).Int().i}}}));
			} else {
				Go_return(p, Str{s: ""})
			}
			return;
		}();
		return <-p;

	};
	// method string
	Method_MiniPerl6__Match.f_string = func(v_self *MiniPerl6__Match, v Capture) Any {

		p := make(chan Any);
		go func() {
			if (v_self.v_bool).Bool().b {
				if v_self.v_capture.Defined().Bool().b {
					Go_return(p, v_self.v_capture)
				} else {
				}
				Go_return(p, Substr(Capture{p: []Any{v_self.v_str, v_self.v_from, Int{i: (v_self.v_to).Int().i - (v_self.v_from).Int().i}}}));
			} else {
				Go_return(p, Str{s: ""})
			}
			return;
		}();
		return <-p;

	};
}

// interfaces for all methods
type from_er interface {
	f_from(Capture) Any;
}
type to_er interface {
	f_to(Capture) Any;
}
type str_er interface {
	f_str(Capture) Any;
}
type bool_er interface {
	f_bool(Capture) Any;
}
type capture_er interface {
	f_capture(Capture) Any;
}
type string_er interface {
	f_string(Capture) Any;
}

// end: MiniPerl6::Match


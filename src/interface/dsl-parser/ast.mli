type var_id = string
type prop = string
type time = Unbound | Bound of int
type value = float
type units = S | Ms | Us | Ns

type kind =
  | Less of time * units
  | Equal of time * units
  | LessOrEqual of time * units

type fm =
  | True
  | False (* shorthand *)
  | Prop of prop
  | Not of fm
  | Or of fm * fm
  | And of fm * fm (* shorthand *)
  | Implies of fm * fm (* shorthand *)
  | Until of kind * fm * fm
  | Since of kind * fm * fm
  | LessThan of tm * tm
  | LessOrEqualThan of tm * tm
  | GreaterThan of tm * tm
  | GreaterOrEqualThan of tm * tm
  | Next of kind * fm (* shorthand *)
  | Prev of kind * fm (* shorthand *)
  | Fall of kind * fm (* shorthand *)
  | Rise of kind * fm (* shorthand *)
  | Always of kind * fm (* shorthand *)
  | Historically of kind * fm (* shorthand *)
  | Eventually of kind * fm (* shorthand *)
  | PastEventually of kind * fm (* shorthand *)

and interval = Interval of tm * tm

and tm =
  | Constant of value
  | C of time * units
  | FPlus of tm * tm
  | FTimes of tm * tm
  | Duration of interval * fm

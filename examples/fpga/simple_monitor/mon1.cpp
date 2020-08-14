
#include <RTML_buffer.h>
#include <RTML_reader.h>

#include <rmtld3/reader.h>
#include <rmtld3/rmtld3.h>

#include <rmtld3/macros.h>

template <typename T> class Eval_until_less_1 {
public:
  static three_valued_type eval_phi1(T &trace, timespan& t) { return T_TRUE; };
  static three_valued_type eval_phi2(T &trace, timespan& t) {
    proposition p = 1;
    auto sf = prop<T>(trace, p, t);
    return b3_not(sf);
  };
};

/*template <typename T> class Eval_until_less_2 {
public:
  static three_valued_type eval_phi1(T &trace, timespan& t) { return T_TRUE; };
  static three_valued_type eval_phi2(T &trace, timespan& t) {
    auto sf = until_less<T, Eval_until_less_1<T>>(trace, t);
    return sf;
  };
};*/

three_valued_type demo(RTML_buffer<int, 100> &buf) {
  three_valued_type _out;

  typedef RMTLD3_reader<RTML_reader<RTML_buffer<int, 100>>> trace_t;

  // monitor trace
  trace_t trace = trace_t(buf, 10.);

  //_out = _mon0_compute(env,0);

  /*Event<int> e;
  buf.pop(e);

  if (e.getData() == 10)
          _out = T_FALSE;
  else
          _out = T_TRUE;
*/
  trace.synchronize();

  // proposition 1
  // auto _mon0_compute = [](Environment &env, timespan t) -> three_valued_type
  // { return env.evaluate(env, 1, t); };

  // proposition 1 or 2
  // auto _mon0_compute = [](Environment &env, timespan t) -> three_valued_type
  // { return b3_or (env.evaluate(env, 1, t), env.evaluate(env, 2, t)); };

  // proposition 1 or 2 or 3
  // auto _mon0_compute = [](Environment &env, timespan t) -> three_valued_type
  // { return b3_or (env.evaluate(env, 1, t + 1.), b3_or (env.evaluate(env, 2,
  // t), env.evaluate(env, 3, t + 2.))); };

  // proposition 1 or 2 or 3 or 4
  // auto _mon0_compute = [](Environment &env, timespan t) -> three_valued_type
  // { return b3_or (env.evaluate(env, 1, t + 1.), b3_or (env.evaluate(env, 2,
  // t), b3_or (env.evaluate(env, 3, t), env.evaluate(env, 4, t + 2.)) )); };

  //
  /*int x[] = {1,2,3,4,3,2,1,4,5,6,7,4,2,6,7,8,9,6,4,3,2,3,5,7,10};
  for(int i=0; i< 20; i++)
  {
          _out = b3_or(_out, env.evaluate(env, x[i], i + 1.));
          _out = b3_or(_out, env.evaluate(env, x[i+2], i + 1.));
  }*/

  auto _mon0_compute = [](trace_t &trace, timespan& t) -> three_valued_type {
    auto sf = until_less<trace_t, Eval_until_less_1<trace_t>>(trace, t);
    return b3_not(sf);
  };

  timespan t = 2;
  _out = _mon0_compute(trace, t);

  // return verdict
  return _out;
}

// just for simulation
int main() {
  printf("Main init!\n");

  RTML_buffer<int, 100> buf;

  Event<int> event1 = Event<int>(1, 2);
  unsigned long int index = 0;
  buf.push(event1);

  Event<int> event2 = Event<int>(1, 5);
  buf.push(event2);

  Event<int> event3 = Event<int>(1, 9);
  buf.push(event3);

  Event<int> event4 = Event<int>(1, 14);
  buf.push(event4);

  // Event<int> event5 = Event<int>(1,20);
  // buf.push(event5);

  // Event<int> event;
  // buf.read(event, index);

  three_valued_type ret = demo(buf);

  printf("_out=%s\n", out_p(ret));

  printf("Main quit!\n");
}

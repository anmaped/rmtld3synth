
#include <RTML_buffer.h>
#include <RTML_reader.h>

#include <rmtld3/reader.h>
#include <rmtld3/rmtld3.h>

struct Environment {
  RMTLD3_reader<RTML_reader<RTML_buffer<int, 100>>> &trace;
  three_valued_type evaluate(struct Environment &env, proposition p,
                             timespan t) {

    Event<int> e;
    Event<int> e_next;

    RTML_reader<RTML_buffer<int, 100ul>>::error_t status = env.trace.read(e);

    if (status == env.trace.AVAILABLE &&
        env.trace.read_next(e_next) == env.trace.AVAILABLE &&
        e.getTime() <= t && t < e_next.getTime()) {
      DEBUGV_RMTLD3("  eval: t=%lu prop=%d - (%d %d) next-> (%d %d)\n", t, p,
                    e.getData(), e.getTime(), e_next.getData(),
                    e_next.getTime());
      return e.getData() == p ? T_TRUE : T_FALSE;
    } else {
      DEBUGV_RMTLD3("  eval: t=%lu prop=%d - (%d %d) nobound\n", t, p,
                    e.getData(), e.getTime());
      return T_UNKNOWN;
    }
  };

  Environment(RMTLD3_reader<RTML_reader<RTML_buffer<int, 100>>> &t)
      : trace(t){};
};

three_valued_type demo(RTML_buffer<int, 100> &buf) {
  three_valued_type _out;

  // monitor
  RMTLD3_reader<RTML_reader<RTML_buffer<int, 100>>> trace =
      RMTLD3_reader<RTML_reader<RTML_buffer<int, 100>>>(buf, 10.);
  struct Environment env = Environment(trace);

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

  auto _mon0_compute = [](Environment &env, timespan t) -> three_valued_type {
    auto sf = [](Environment env, timespan t) -> three_valued_type {
      auto eval_fold = [](Environment env,
                          timespan t) -> std::pair<four_valued_type, timespan> {
        // eval_b lambda function
        auto eval_b = [](Environment env, timespan t,
                         four_valued_type v) -> four_valued_type {
          // eval_i lambda function
          auto eval_i = [](three_valued_type b1,
                           three_valued_type b2) -> four_valued_type {
            return (b2 != T_FALSE)
                       ? b3_to_b4(b2)
                       : ((b1 != T_TRUE) ? b3_to_b4(b1) : FV_SYMBOL);
          };

          // change this (trying to get the maximum complexity)
          // if ( v == FV_SYMBOL )
          //{
          DEBUGV_RMTLD3("  compute phi1\n");
          // compute phi1
          three_valued_type cmpphi1 = [](Environment &env,
                                         timespan t) -> three_valued_type {
            return T_TRUE;
          }(env, t);
          DEBUGV_RMTLD3("  compute phi1 end.\n");

          DEBUGV_RMTLD3("  compute phi2\n");
          // compute phi2
          three_valued_type cmpphi2 = [](Environment &env,
                                         timespan t) -> three_valued_type {
            auto sf = [](Environment &env, timespan t) -> three_valued_type {
              return env.evaluate(env, 1, t);
            }(env, t);
            return b3_not(sf);
          }(env, t);
          DEBUGV_RMTLD3("  compute phi2 end.\n");

          four_valued_type rs = eval_i(cmpphi1, cmpphi2);

          DEBUGV_RMTLD3(" phi1=%s UNTIL phi2=%s\n", out_p(cmpphi1),
                        out_p(cmpphi2));

          if (v == FV_SYMBOL) {
            return rs;
          } else {
            return v;
          }
        };

        env.trace.set(t); // force start at t

        timespan c_time = t;

        four_valued_type symbol = FV_SYMBOL;

        Event<int> event;

        symbol = eval_b(env, c_time, symbol);

        env.trace.debug();

        while (env.trace.pull(event) == env.trace.AVAILABLE) {
          DEBUGV_RMTLD3("%d len=%d\n", c_time, env.trace.length());

          env.trace.read(event);
          c_time = event.getTime();

          if (c_time > 10. + t)
            break;

          symbol = eval_b(env, c_time, symbol);

          env.trace.debug();
        }

        return std::make_pair(symbol, c_time);
      };

      DEBUGV_RMTLD3("BEGIN until_op_less.\n ");

      std::pair<four_valued_type, timespan> eval_c = eval_fold(env, t);

      DEBUGV_RMTLD3("END until_op (%s) enough(%d)=%d .\n ",
                    out_fv(eval_c.first), eval_c.second,
                    eval_c.second < t + 10.);

      return (eval_c.first == FV_SYMBOL)
                 ? ((eval_c.second < t + 10.) ? T_UNKNOWN : T_FALSE)
                 : b4_to_b3(eval_c.first);
    }

    (env, t);
    return b3_not(sf);
  };

  _out = _mon0_compute(env, 0);

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

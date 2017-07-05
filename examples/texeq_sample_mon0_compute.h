/*
 * automatically generated code from rmtld3synth Git version v0.2-alpha-46-g38ff318 (38ff318181783ef464b346f42a37ef72d08340a8)
 * cmd: ./rmtld3synth --synth-cpp11 --input-latexeq "(a \rightarrow ((a \lor b) \until_{<10} c)) \land \int^{10} (c \lor d) < 4"
 */

#ifndef _MON0_COMPUTE_H_
#define _MON0_COMPUTE_H_

#include "rmtld3.h"

auto _mon0_compute = [](struct Environment &env, timespan t) mutable -> three_valued_type { auto sf = [](struct Environment &env, timespan t) mutable -> three_valued_type { auto sf1 = [](struct Environment &env, timespan t) mutable -> three_valued_type { auto sf = [](struct Environment &env, timespan t) mutable -> three_valued_type { auto sf1 = [](struct Environment &env, timespan t) mutable -> three_valued_type { auto sf = [](struct Environment &env, timespan t) mutable -> three_valued_type { return env.evaluate(env, 5, t); }(env,t); return b3_not (sf); }(env,t); auto sf2 = [](struct Environment env, timespan t) -> three_valued_type
{
auto eval_fold = []( struct Environment env, timespan t, TraceIterator< int > iter) -> four_valued_type
{

  // eval_b lambda function
  auto eval_b = []( struct Environment env, timespan t, four_valued_type v ) -> four_valued_type
  {
    // eval_i lambda function
    auto eval_i = [](three_valued_type b1, three_valued_type b2) -> four_valued_type
    {
      return (b2 != T_FALSE) ? b3_to_b4(b2) : ( (b1 != T_TRUE && b2 == T_FALSE) ? b3_to_b4(b1) : FV_SYMBOL );
    };

    // change this (trying to get the maximum complexity)
    //if ( v == FV_SYMBOL )
    //{
      DEBUGV_RTEMLD3("  compute phi1\n");
      // compute phi1
      three_valued_type cmpphi1 = [](struct Environment &env, timespan t) mutable -> three_valued_type { auto sf1 = [](struct Environment &env, timespan t) mutable -> three_valued_type { return env.evaluate(env, 5, t); }(env,t); auto sf2 = [](struct Environment &env, timespan t) mutable -> three_valued_type { return env.evaluate(env, 4, t); }(env,t); return b3_or (sf1, sf2); }(env, t);

      DEBUGV_RTEMLD3("  compute phi2\n");
      // compute phi2
      three_valued_type cmpphi2 = [](struct Environment &env, timespan t) mutable -> three_valued_type { return env.evaluate(env, 2, t); }(env, t);

      four_valued_type rs = eval_i(cmpphi1, cmpphi2);

      DEBUGV_RTEMLD3(" phi1=%s UNTIL phi2=%s\n", out_p(cmpphi1), out_p(cmpphi2) );

    if ( v == FV_SYMBOL )
    {
      return rs;
    }
    else
    {
      return v;
    }
  };

  DEBUGV_RTEMLD3("BEGIN until_op.\n\n ");
  iter.debug();

  ASSERT_RMTLD3( t == iter.getLowerAbsoluteTime() );

  auto cos = iter.getBegin();

  four_valued_type s = std::accumulate(
    iter.begin(),
    iter.end(),
    std::pair<four_valued_type, timespan>(FV_SYMBOL, t),
      [&env, &cos, eval_b]( const std::pair<four_valued_type, timespan> a, Event< int > e ) {

        DEBUGV_RTEMLD3("  until++ (%s)\n", out_fv(a.first));

        count_until_iterations += 1;

        /* update the new state based on the sub_k calculate iterator.
         * optimization step: update current index to avoid re-read/evaluate events several times
         */
        env.state = std::make_pair ( cos,  a.second);
        cos++;

        return std::make_pair( eval_b( env, a.second, a.first ), a.second + e.getTime() );
      }
  ).first;

  return s;
};


// sub_k function defines a sub-trace
auto sub_k = []( struct Environment env, timespan t) -> TraceIterator< int >
{

  // use env.state to speedup the calculation of the new bounds
  TraceIterator< int > iter = TraceIterator< int > (env.trace, env.state.first, 0, env.state.first, env.state.second, 0, env.state.second );

  // to use the iterator for both searches we use one reference
  TraceIterator< int > &it = iter;

  ASSERT_RMTLD3( t == iter.getLowerAbsoluteTime() );

  auto lower = env.trace->searchIndexForwardUntil( it, t);
  auto upper = env.trace->searchIndexForwardUntil( it, (t + 10.) - 1 );

  // set TraceIterator for interval [t, t+10.[
  it.setBound(lower, upper);

  // return iterator ... interval length may be zero
  return it;
};

TraceIterator< int > subk = sub_k(env, t);

four_valued_type eval_c = eval_fold(env, t, subk );

DEBUGV_RTEMLD3("END until_op (%s) enough(%d) .\n\n ", out_fv(eval_c), subk.getEnoughSize() );

return ( eval_c == FV_SYMBOL ) ?
  ( ( !subk.getEnoughSize() ) ? T_UNKNOWN : T_FALSE )
:
  b4_to_b3(eval_c);
}

(env,t); return b3_or (sf1, sf2); }(env,t); return b3_not (sf); }(env,t); auto sf2 = [](struct Environment &env, timespan t) mutable -> three_valued_type { auto sf = [](struct Environment &env, timespan t) mutable -> three_valued_type { return [](struct Environment env, timespan t) -> three_valued_type { auto tr1 = [](struct Environment env, timespan t) -> duration {

auto eval_eta =  [](struct Environment env, timespan t, timespan t_upper, TraceIterator< int > iter) -> duration
{
  auto indicator_function = [](struct Environment env, timespan t) -> duration {
    auto formula = [](struct Environment &env, timespan t) mutable -> three_valued_type { auto sf1 = [](struct Environment &env, timespan t) mutable -> three_valued_type { return env.evaluate(env, 2, t); }(env,t); auto sf2 = [](struct Environment &env, timespan t) mutable -> three_valued_type { return env.evaluate(env, 1, t); }(env,t); return b3_or (sf1, sf2); }(env, t);

    return (formula == T_TRUE)? std::make_pair (1,false) : ( (formula == T_FALSE)? std::make_pair (0,false) : std::make_pair (0,true)) ;

  };


  // compare if t is equal to the lower bound
  auto lower = iter.getLowerAbsoluteTime();
  // compare if t is equal to the upper bound
  auto upper = iter.getUpperAbsoluteTime();

  timespan val1 = ( t == lower )? 0 : t - lower;
  timespan val2 = ( t_upper == upper )? 0 : t_upper - upper;

  DEBUGV_RTEMLD3("dur lower(%ld) upper(%ld)\n", val1, val2);

  auto cum = lower;

  // lets do the fold over the trace
  return std::accumulate(
    iter.begin(),
    iter.end(),
    std::make_pair (make_duration (0, false), (timespan)lower), // initial fold data (duration starts at 0)
    [&env, val1, val2, &cum, t, t_upper, indicator_function]( const std::pair<duration,timespan> p, Event< int > e )
    {
      auto d = p.first;

      auto t_begin = cum;
      auto t_end = t_begin + e.getTime();
      cum = t_end;

      auto cond1 = t_begin <= t && t < t_end;
      auto cond2 = t_begin <= t_upper && t_upper < t_end;

      auto valx = ((cond1)? val1 : 0 ) + ((cond2)? val2 : 0);

      auto x = indicator_function(env, p.second);

      DEBUGV_RTEMLD3("dur=%f bottom=%d\n", d.first + (x.first * ( e.getTime() - valx )), d.second || x.second);

      return std::make_pair (make_duration (d.first + (x.first * ( e.getTime() - valx )), d.second || x.second), p.second + e.getTime());
    }
  ).first;

};

// sub_k function defines a sub-trace
auto sub_k = []( struct Environment env, timespan t, timespan t_upper) -> TraceIterator< int >
{

  // use env.state to speedup the calculation of the new bounds
  TraceIterator< int > iter = TraceIterator< int > ( env.trace, env.state.first, 0, env.state.first, env.state.second, 0, env.state.second );

  // to use the iterator for both searches we use one reference
  TraceIterator< int > &it = iter;

  ASSERT_RMTLD3( t == iter.getLowerAbsoluteTime() );

  auto lower = env.trace->searchIndexForwardUntil( it, t);
  auto upper = env.trace->searchIndexForwardUntil( it, t_upper - 1 );

  // set TraceIterator for interval [t, t + di[
  it.setBound(lower, upper);

  // return iterator ... interval length may be zero
  return it;
};

auto t_upper = t + make_duration(10.,false).first;

return eval_eta(env, t, t_upper, sub_k(env, t, t_upper));

}(env,t); auto tr2 = make_duration(4.,false); return b3_lessthan (tr1, tr2); }(env,t); }(env,t); return b3_not (sf); }(env,t); return b3_or (sf1, sf2); }(env,t); return b3_not (sf); };

#endif //_MON0_COMPUTE_H_

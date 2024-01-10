-module(proposer).
-export([start/6]).

-define(timeout, 2000).
-define(backoff, 10).

start(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
  spawn(fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId, Main) end).

init(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
  timer:sleep(Sleep),
  Begin = erlang:monotonic_time(),
  Round = order:first(Name),
  {Decision, LastRound} = round(Name, ?backoff, Round, Proposal, Acceptors, PanelId),
  End = erlang:monotonic_time(),
  Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
  io:format("[Proposer ~w] DECIDED ~w in round ~w after ~w ms~n", 
             [Name, Decision, LastRound, Elapsed]),
  Main ! done,
  PanelId ! stop.

round(Name, Backoff, Round, Proposal, Acceptors, PanelId) ->
  io:format("[Proposer ~w] Phase 1: round ~w proposal ~w~n", 
             [Name, Round, Proposal]),
  % Update gui
  PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Proposal},
  case ballot(Name, Round, Proposal, Acceptors, PanelId) of
    {ok, Value} ->
      {Value, Round};
    abort ->
      timer:sleep(rand:uniform(Backoff)),
      Next = order:inc(Round),
      round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId)
  end.

ballot(Name, Round, Proposal, Acceptors, PanelId) ->
  prepare(Round, Acceptors),
  Quorum = (length(Acceptors) div 2) + 1,
  MaxVoted = order:null(), % max voted round by the acceptors
  case collect(Quorum,Quorum, Round, MaxVoted, Proposal) of
    {accepted, Value} ->
      io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n", 
                 [Name, Round, Value, Proposal]),
      % update gui
      PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Value},
      accept(Round, Value, Acceptors),
      case vote(Quorum,Quorum, Round) of
        ok ->
          {ok, Value};
        abort ->
          abort
      end;
    abort ->
      abort
  end.

collect(0,_, _, _, Proposal) ->
  {accepted, Proposal};
collect(_,0, _, _, Proposal) ->
  {abort, Proposal};
collect(N,Sorry, Round, MaxVoted, Proposal) ->
  receive 
    {promise, Round, _, na} ->
      collect(N-1,Sorry, Round, MaxVoted, Proposal);
    {promise, Round, Voted, Value} ->
      case order:gr(Voted, Round) of
        true ->
          collect(N-1,Sorry, Round, Voted, Value);
        false ->
          collect(N-1,Sorry, Round, MaxVoted, Proposal)
      end;
    {promise, _, _,  _} ->
      collect(N,Sorry, Round, MaxVoted, Proposal);
    {sorry, {prepare, Round}} ->
      collect(N-1,Sorry-1, Round, MaxVoted, Proposal);
    {sorry, _} ->
      collect(N,Sorry, Round, MaxVoted, Proposal)
  after ?timeout ->
    abort
  end.

% vote sorry messages improving
vote(0,_, _) ->
  ok;
vote(_,0,_) ->
  abort;

vote(N,Sorry, Round) ->
  receive
    {vote, Round} ->
      vote(N-1,Sorry, Round);
    {vote, _} ->
      vote(N,Sorry, Round);
    {sorry, {accept, Round}} ->
      vote(N,Sorry-1, Round);
    {sorry, _} ->
      vote(N,Sorry, Round)
  after ?timeout ->
    abort
  end.

prepare(Round, Acceptors) ->
  Fun = fun(Acceptor) -> 
    send(Acceptor, {prepare, self(), Round}) 
  end,
  lists:foreach(Fun, Acceptors).

accept(Round, Proposal, Acceptors) ->
  Fun = fun(Acceptor) -> 
    send(Acceptor, {accept, self(), Round, Proposal}) 
  end,
  lists:foreach(Fun, Acceptors).

send(Name, Message) ->
  Name ! Message.

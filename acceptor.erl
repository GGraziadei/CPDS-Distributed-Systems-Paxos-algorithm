-module(acceptor).
-export([start/2]).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).
        
init(Name, PanelId) ->
  Promised = order:null(), 
  Voted = order:null(),
  Value = na,
  acceptor(Name, Promised, Voted, Value, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
          store_state(Name, Promised, Voted, Value),
          Proposer ! {promise, Round, Voted, Value},               
      io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                 [Name, Round, Voted, Value]), 
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]), 
                     "Promised: " ++ io_lib:format("~p", [Round]), Colour},
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          store_state(Name, Promised, Voted, Value),
          Proposer ! {sorry, {prepare, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;

    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promised) of %equals to check if the promised == Round
        true ->
          store_state(Name, Promised, Voted, Value),
          Proposer ! {vote, Round},
          case order:goe(Round, Voted) of
            true ->
      io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                 [Name, Promised, Round, Proposal]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]), 
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
              acceptor(Name, Promised, Round, Value, PanelId)
          end;                            
        false ->
          store_state(Name, Promised, Voted, Value),
          Proposer ! {sorry, {accept, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      ok
  end.

store_state(Name, Promised, Voted, Value)->
  pers:open(Name),
  pers:store(Name, Promised, Voted, Value, 1),
  pers:close(Name).

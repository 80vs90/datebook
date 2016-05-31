-module(datebook_server).
-behaviour(gen_server).

-export([start/0, schedule_event/1, retrieve_events/1, cancel_event/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(event, {id, date, time, description}).

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% datebook api
schedule_event(Event) -> gen_server:call(?MODULE, {schedule, Event}).
retrieve_events(Date) -> gen_server:call(?MODULE, {retrieve, Date}).
cancel_event(EventId) -> gen_server:call(?MODULE, {cancel, EventId}).

% gen_server stuff
init([]) ->
    Datebook = [],
    {ok, Datebook}.

handle_call({schedule, {{Date, Time}, Description}}, _From, Datebook) ->
    {reply, ok, Datebook};
handle_call({retrieve, Date}, _From, Datebook) ->
    {reply, ok, Datebook};
handle_call({cancel, EventId}, _From, Datebook) ->
    {reply, ok, Datebook};
handle_call(_Message, _From, Datebook) ->
    {reply, error, Datebook}.

handle_cast(_Message, Datebook) -> {noreply, Datebook}.
handle_info(_Message, Datebook) -> {noreply, Datebook}.
terminate(_Reason, _Datebook) -> ok.
code_change(_OldVersion, Datebook, _Extra) -> {ok, Datebook}.

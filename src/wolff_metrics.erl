-module(wolff_metrics).


-export([
    inflight_change/2,
    queuing_change/2,
    dropped_inc/1,
    dropped_inc/2,
    dropped_queue_full_inc/1,
    dropped_queue_full_inc/2,
    failed_inc/1,
    failed_inc/2,
    retried_inc/1,
    retried_inc/2,
    retried_failed_inc/1,
    retried_failed_inc/2,
    retried_success_inc/1,
    retried_success_inc/2,
    success_inc/1,
    success_inc/2
]).

%%    matched_inc/1,
%%    matched_inc/2,

%% Gauges (value can go both up and down):
%% --------------------------------------

%% @doc Count of messages that are currently queuing. [Gauge]
queuing_change(Config, Val) ->
    telemetry:execute([wolff, queuing],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

%% @doc Count of messages that were sent asynchronously but ACKs are not
%% received. [Gauge]
inflight_change(Config, Val) ->
    telemetry:execute([wolff, inflight],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

%% Counters (value can only got up):
%% --------------------------------------

%% @doc Count of messages dropped.
dropped_inc(Config) ->
    dropped_inc(Config, 1).

dropped_inc(Config, Val) ->
    telemetry:execute([wolff, dropped],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

%% @doc Count of messages dropped due to the queue is full.
dropped_queue_full_inc(Config) ->
    dropped_queue_full_inc(Config, 1).

dropped_queue_full_inc(Config, Val) ->
    telemetry:execute([wolff, dropped_queue_full],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

%% @doc Count of this bridge is matched and queried.
%% TODO Move to higher level. Maybe should not be done in resource worker
% matched_inc(Config) ->
%     matched_inc(Config, 1).

% matched_inc(Config, Val) ->
%     telemetry:execute([wolff, matched],
%                       #{counter_inc => Val},
%                       telemetry_meta_data(Config)).


%% @doc Times of retried.
retried_inc(Config) ->
    retried_inc(Config, 1).

retried_inc(Config, Val) ->
    telemetry:execute([wolff, retried],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

%% @doc Count of messages that sent failed.
failed_inc(Config) ->
    failed_inc(Config, 1).

failed_inc(Config, Val) ->
    telemetry:execute([wolff, failed],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

%%% @doc Count of messages that sent failed.
retried_failed_inc(Config) ->
    retried_failed_inc(Config, 1).

retried_failed_inc(Config, Val) ->
    telemetry:execute([wolff, retried_failed],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

 %% @doc Count messages that where sucessfully sent after a fail
 retried_success_inc(Config) ->
     retried_success_inc(Config, 1).

 retried_success_inc(Config, Val) ->
     telemetry:execute([wolff, retried_success],
                       #{counter_inc => Val},
                       telemetry_meta_data(Config)).

%% @doc Count of messages that sent successfully.
success_inc(Config) ->
    success_inc(Config, 1).

success_inc(Config, Val) ->
    telemetry:execute([wolff, success],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

telemetry_meta_data(Config) ->
    maps:get(telemetry_meta_data, Config, #{data => default_data}).

